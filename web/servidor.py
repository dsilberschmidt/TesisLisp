#!/usr/bin/env python3
"""Servidor web minimo de LGC (1989). Solo biblioteca estandar.

  python3 web/servidor.py            # http://localhost:8080
  PORT=9000 python3 web/servidor.py

GET  /          la pagina
GET  /bases     las bases de PROBLEMA/FUENTES.LSP, tal como se escribieron en 1989
POST /consulta  {"base": "...", "meta": "...", "respuestas": 5, "limite": 0,
                 "monitor": false, "anchura": false}  ->  {"salida": "..."}

Cada consulta corre en un proceso SBCL nuevo y descartable, con topes de
tiempo, CPU y memoria, de a una por vez. El proceso solo ejecuta
(tlc::consulta-env): sin toplevel, sin Lisp anidado, lectura sin #.
"""
import json
import os
import re
import resource
import subprocess
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

RAIZ = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PAGINA = os.path.join(RAIZ, "web", "index.html")
MAX_BASE, MAX_META, MAX_CUERPO = 4000, 400, 8000
TIEMPO = 20                      # segundos de busqueda (dentro de Lisp)
UNA_A_LA_VEZ = threading.Semaphore(1)


def leer_bases():
    """Extrae (nombre, texto) de cada (setq nombre '( ... )) de FUENTES.LSP."""
    with open(os.path.join(RAIZ, "PROBLEMA", "FUENTES.LSP"), encoding="latin-1") as f:
        src = f.read().replace("\r", "").replace("\x1a", "")
    bases = []
    for m in re.finditer(r"\(setq\s+([\w-]+)\s+'", src):
        i, nivel = m.end(), 0
        for j in range(i, len(src)):
            nivel += {"(": 1, ")": -1}.get(src[j], 0)
            if nivel == 0:
                bases.append({"nombre": m.group(1), "texto": src[i:j + 1]})
                break
    return bases


BASES = leer_bases()


def limites():
    resource.setrlimit(resource.RLIMIT_CPU, (TIEMPO + 10, TIEMPO + 10))
    resource.setrlimit(resource.RLIMIT_AS, (1 << 30, 1 << 30))      # 1 GB
    resource.setrlimit(resource.RLIMIT_NOFILE, (64, 64))
    # Sin RLIMIT_NPROC: cuenta todos los procesos del usuario en la maquina y,
    # agotado, SBCL no puede crear el hilo del tope de tiempo. En el contenedor
    # lo cubre el limite de procesos del contenedor.


def consultar(base, meta, respuestas=5, limite=0, monitor=False, anchura=False):
    env = {"PATH": os.environ.get("PATH", "/usr/bin:/bin"),
           "TLC_BASE": base, "TLC_META": meta,
           "TLC_RESPUESTAS": str(max(1, min(int(respuestas), 20))),
           "TLC_LIMITE": str(max(0, min(int(limite), 50))),
           "TLC_MONITOR": "1" if monitor else "0",
           "TLC_ANCHURA": "1" if anchura else "0",
           "TLC_TIEMPO": str(TIEMPO)}
    cmd = ["sbcl", "--dynamic-space-size", "512", "--noinform", "--non-interactive",
           "--no-sysinit", "--no-userinit", "--load", "loader.lisp",
           "--eval", "(tlc::consulta-env)"]
    with UNA_A_LA_VEZ:
        try:
            r = subprocess.run(cmd, cwd=RAIZ, env=env, capture_output=True, text=True,
                               stdin=subprocess.DEVNULL, preexec_fn=limites,
                               timeout=TIEMPO + 15)
        except subprocess.TimeoutExpired:
            print("consulta: proceso cortado por tiempo", flush=True)
            return "[tiempo agotado]"
    if "=====SALIDA=====" in r.stdout:
        return r.stdout.split("=====SALIDA=====", 1)[1].strip("\n")[:200_000]
    print(f"consulta: fallo rc={r.returncode}: {r.stderr[-300:]}", flush=True)
    return "[la consulta excedio los recursos disponibles]"


class Manejador(BaseHTTPRequestHandler):
    def responder(self, codigo, cuerpo, tipo="application/json; charset=utf-8"):
        datos = cuerpo.encode("utf-8")
        self.send_response(codigo)
        self.send_header("Content-Type", tipo)
        self.send_header("Content-Length", str(len(datos)))
        self.send_header("X-Content-Type-Options", "nosniff")
        self.end_headers()
        self.wfile.write(datos)

    def do_GET(self):
        if self.path in ("/", "/index.html"):
            with open(PAGINA, encoding="utf-8") as f:
                self.responder(200, f.read(), "text/html; charset=utf-8")
        elif self.path == "/bases":
            self.responder(200, json.dumps(BASES, ensure_ascii=False))
        elif self.path == "/salud":
            self.responder(200, '{"ok": true}')
        else:
            self.responder(404, '{"error": "no existe"}')

    def do_POST(self):
        if self.path != "/consulta":
            return self.responder(404, '{"error": "no existe"}')
        try:
            n = int(self.headers.get("Content-Length", "0"))
            if n > MAX_CUERPO:
                return self.responder(413, '{"error": "consulta demasiado grande"}')
            q = json.loads(self.rfile.read(n) or b"{}")
            base, meta = str(q.get("base", "")), str(q.get("meta", ""))
            if not base.strip() or not meta.strip():
                return self.responder(400, '{"error": "faltan base o meta"}')
            if len(base) > MAX_BASE or len(meta) > MAX_META:
                return self.responder(413, '{"error": "base o meta demasiado larga"}')
            salida = consultar(base, meta, q.get("respuestas", 5), q.get("limite", 0),
                               bool(q.get("monitor")), bool(q.get("anchura")))
            self.responder(200, json.dumps({"salida": salida}, ensure_ascii=False))
        except (ValueError, TypeError):
            self.responder(400, '{"error": "consulta mal formada"}')


if __name__ == "__main__":
    puerto = int(os.environ.get("PORT", "8080"))
    print(f"LGC en http://localhost:{puerto}")
    ThreadingHTTPServer(("", puerto), Manejador).serve_forever()
