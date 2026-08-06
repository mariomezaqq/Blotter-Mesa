// Genera cmf/mapeo_nemo_cmf.csv cruzando FONDOS_DB (index.html) contra el
// catalogo de fondos de la CMF (cmf/catalogo_fondos_referencia.csv), por
// nombre normalizado + serie exacta, con fallback a solapamiento de tokens
// >= 0.7. Correr de nuevo cuando se agreguen fondos nuevos a FONDOS_DB:
//   node cmf/build_mapeo.mjs
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const DIR = path.dirname(fileURLToPath(import.meta.url));
const ROOT = path.dirname(DIR);

const COMBINING_MARKS = new RegExp('[\\u0300-\\u036f]', 'g');

function norm(s) {
  return (s || '')
    .toUpperCase()
    .normalize('NFD').replace(COMBINING_MARKS, '')
    .replace(/FONDO DE INVERSION\S*/g, '')
    .replace(/FONDO MUTUO\S*/g, '')
    .replace(/[^A-Z0-9 ]/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
}
function tokens(s) { return norm(s).split(' ').filter(Boolean); }

function parseCsvLine(line) {
  const cols = line.match(/(".*?"|[^,]+)(?=,|$)/g) || [];
  return cols.map(c => c.replace(/^"|"$/g, ''));
}

function csvField(v) {
  const s = String(v ?? '');
  return /[",\n]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s;
}

const html = fs.readFileSync(path.join(ROOT, 'index.html'), 'utf8');
const m = html.match(/const FONDOS_DB = (\[.*?\]);/s);
if (!m) throw new Error('No se encontro FONDOS_DB en index.html');
const db = JSON.parse(m[1]);

const csvRaw = fs.readFileSync(path.join(DIR, 'catalogo_fondos_referencia.csv'), 'utf8');
const lines = csvRaw.split(/\r?\n/).filter(Boolean);
const rows = [];
for (let i = 1; i < lines.length; i++) {
  const c = parseCsvLine(lines[i]);
  if (c.length < 7) continue;
  const [nombre, run, serie, tipoentidad, row] = c;
  rows.push({ nombre, run, serie, tipoentidad, row, nombreNorm: norm(nombre) });
}

const catByNorm = new Map();
for (const r of rows) {
  if (!catByNorm.has(r.nombreNorm)) catByNorm.set(r.nombreNorm, []);
  catByNorm.get(r.nombreNorm).push(r);
}

let exact = 0, tokenMatch = 0, noMatch = 0;
const out = [];
for (const f of db) {
  const fn = norm(f.nombre);
  let matched = null;

  if (catByNorm.has(fn)) {
    const cands = catByNorm.get(fn);
    matched = cands.find(c => (c.serie || '').toUpperCase() === (f.serie || '').toUpperCase()) || cands[0];
    if (matched) exact++;
  }

  if (!matched) {
    const ft = tokens(f.nombre);
    let best = null, bestScore = 0;
    for (const r of rows) {
      const rt = tokens(r.nombre);
      if (!rt.length || !ft.length) continue;
      const inter = rt.filter(t => ft.includes(t)).length;
      const score = Math.min(inter / rt.length, inter / ft.length);
      if (score > bestScore) { bestScore = score; best = r; }
    }
    if (best && bestScore >= 0.7) { matched = best; tokenMatch++; }
  }

  if (!matched) { noMatch++; continue; }
  out.push({
    nemo: f.nemo, run: matched.run, serie: matched.serie,
    tipoentidad: matched.tipoentidad, row: matched.row, nombre_cmf: matched.nombre
  });
}

const header = 'nemo,run,serie,tipoentidad,row,nombre_cmf';
const body = out.map(r => [r.nemo, r.run, r.serie, r.tipoentidad, r.row, r.nombre_cmf].map(csvField).join(',')).join('\n');
fs.writeFileSync(path.join(DIR, 'mapeo_nemo_cmf.csv'), header + '\n' + body + '\n', 'utf8');

console.log(`FONDOS_DB: ${db.length} | catalogo CMF: ${rows.length}`);
console.log(`Matches exactos: ${exact} | por tokens: ${tokenMatch} | sin match: ${noMatch}`);
console.log(`Escrito cmf/mapeo_nemo_cmf.csv con ${out.length} fondos.`);
