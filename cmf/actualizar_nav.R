# =============================================================================
# ACTUALIZAR NAV (valor cuota) DESDE LA CMF -> Supabase del Blotter
# Corre diario via GitHub Actions (.github/workflows/nav-cmf.yml). Por cada
# fondo de cmf/mapeo_nemo_cmf.csv, scrapea el ultimo valor cuota publicado en
# la CMF (cmf/scraper.R) y lo sube a la tabla `valores_cuota` (nemo, fecha,
# valor, fondo_nombre), que ya consume el Blotter para el precio "a NAV" de
# las ordenes (getValorCuotaNAV en index.html).
# =============================================================================
suppressMessages({ library(httr); library(rvest); library(dplyr); library(stringr)
                   library(lubridate); library(jsonlite) })

base <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
if (!file.exists(file.path(base, "scraper.R"))) base <- file.path(getwd(), "cmf")
setwd(base)
source("scraper.R")

token   <- Sys.getenv("CMF_RECAPTCHA_TOKEN", "")
cookies <- Sys.getenv("CMF_COOKIES", "")
sb_url  <- Sys.getenv("SUPABASE_URL", "")
sb_key  <- Sys.getenv("SUPABASE_KEY", "")

if (!nzchar(sb_url) || !nzchar(sb_key)) stop("Faltan SUPABASE_URL / SUPABASE_KEY.")
if (!nzchar(token)) message("AVISO: sin CMF_RECAPTCHA_TOKEN. Los fondos FIRES/FINRE van a fallar (los RGFMU no lo necesitan).")

mapeo <- read.csv("mapeo_nemo_cmf.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                   colClasses = c(run = "character", serie = "character", row = "character"))
message("Fondos a actualizar: ", nrow(mapeo))

hasta <- Sys.Date() - 1
desde <- hasta - 15

scrapear_con_reintento <- function(fondo, n = 3) {
  for (intento in seq_len(n)) {
    h <- tryCatch(scrapear_fondo(fondo, desde, hasta, token, cookies),
                  error = function(e) NULL)
    if (!is.null(h) && nrow(h) > 0) return(h)
    if (intento < n) Sys.sleep(2 * intento)
  }
  NULL
}

filas <- list(); fallidos <- character()
for (i in seq_len(nrow(mapeo))) {
  r <- mapeo[i, ]
  fondo <- list(run = r$run, serie = r$serie, row = if (is.na(r$row)) "" else r$row, tipoentidad = r$tipoentidad)

  h <- scrapear_con_reintento(fondo)
  if (is.null(h)) { fallidos <- c(fallidos, r$nemo); next }

  ult <- h[order(h$fecha), ][nrow(h), ]
  filas[[length(filas) + 1]] <- list(
    nemo = r$nemo, fecha = as.character(ult$fecha[1]),
    valor = as.numeric(ult$valor_cuota[1]), fondo_nombre = r$nombre_cmf
  )
  Sys.sleep(0.6)
}

message("OK: ", length(filas), " / ", nrow(mapeo), " fondos scrapeados.",
        if (length(fallidos)) paste0(" Fallidos: ", paste(head(fallidos, 20), collapse = ", "),
                                     if (length(fallidos) > 20) "..." else "") else "")

if (!length(filas)) stop("Ningun fondo scrapeado. Abortando sin subir nada (posible token vencido).")

# Dedup por nemo (last-wins): FONDOS_DB tiene nemos duplicados en algunos fondos
# (bug preexistente del catalogo, no de este script) y Supabase rechaza un upsert
# que intente afectar la misma fila (nemo, fecha) dos veces en un mismo batch.
nemos_vistos <- character()
filas_dedup <- list()
for (f in rev(filas)) {
  if (f$nemo %in% nemos_vistos) next
  nemos_vistos <- c(nemos_vistos, f$nemo)
  filas_dedup[[length(filas_dedup) + 1]] <- f
}
if (length(filas_dedup) < length(filas))
  message("Nemos duplicados en el catalogo: ", length(filas) - length(filas_dedup), " fila(s) descartada(s) antes de subir.")
filas <- filas_dedup

BATCH <- 300
for (i in seq(1, length(filas), by = BATCH)) {
  lote <- filas[i:min(i + BATCH - 1, length(filas))]
  resp <- tryCatch(
    POST(
      url = paste0(sb_url, "/rest/v1/valores_cuota?on_conflict=nemo,fecha"),
      add_headers(apikey = sb_key, Authorization = paste("Bearer", sb_key),
                  `Content-Type` = "application/json",
                  Prefer = "resolution=merge-duplicates,return=minimal"),
      body = toJSON(lote, auto_unbox = TRUE)
    ),
    error = function(e) { message("Error de red subiendo lote: ", e$message); NULL }
  )
  if (is.null(resp) || status_code(resp) >= 300) {
    msg <- if (is.null(resp)) "sin respuesta" else content(resp, "text", encoding = "UTF-8")
    stop("Error subiendo a Supabase: ", msg)
  }
}
message("Subido a valores_cuota: ", length(filas), " fondos (", format(Sys.time(), "%Y-%m-%d %H:%M"), ").")
