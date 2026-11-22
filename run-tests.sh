#!/usr/bin/env bash
# run-tests.sh — regresion batch de TesisLisp (SBCL, no interactivo).
# Cada caso: BASE|GOAL|LIMITE-PROFUNDIDAD|TEXTO-ESPERADO (busqueda literal).
# Esperados proposicionales verificados contra un oraculo independiente
# (chequeo de modelos por fuerza bruta): "Si." = implicado; "No hay" = no.
# Los de primer orden verifican la respuesta extraida.
# Corre en serie: en paralelo las cargas compiten por los .fasl.
set -u
cd "$(dirname "$0")"
TIMEOUT_SECS=${TIMEOUT_SECS:-60}
PASS=0; FAIL=0
while IFS='|' read -r base goal limite expect; do
  [ -z "$base" ] && continue
  out=$(echo n | timeout "$TIMEOUT_SECS" sbcl --non-interactive \
      --load loader.lisp \
      --eval "(in-package :tlc)" \
      --eval "(off flag-monitor flag-interrupcion)" \
      --eval '(load-tlc "PROBLEMA/FUENTES.LSP")' \
      --eval "(compilar $base 'obj-test)" \
      --eval "(setq base-interna (armar-clausulas obj-test))" \
      --eval "(setq goal-interno (armar-goal (let ((*readtable* *tlc-readtable*)) (read-from-string \"$goal\"))))" \
      --eval "(setq limite-profundidad $limite)" \
      --eval "(soporte base-interna goal-interno)" 2>&1)
  rc=$?
  ans=$(printf '%s\n' "$out" | sed -n '/FUENTES-OK/,$p' | sed 's/^FUENTES-OK *//' | tr '\n' ' ')
  if [ $rc -ne 124 ] && printf '%s' "$ans" | grep -qF -- "$expect"; then
    PASS=$((PASS+1))
  else
    [ $rc -eq 124 ] && ans="TIMEOUT (${TIMEOUT_SECS}s)"
    echo "FAIL  $base $goal  esperado [$expect]  obtenido [${ans:0:160}]"
    FAIL=$((FAIL+1))
  fi
done <<'CASOS'
dama1|(hay habitacion1 dama)|8|No hay (mas) respuestas
dama1|(hay habitacion1 tigre)|8|Si.
dama1|(hay habitacion2 dama)|8|Si.
dama1|(hay habitacion2 tigre)|8|No hay (mas) respuestas
dama2|(hay habitacion1 dama)|8|No hay (mas) respuestas
dama2|(hay habitacion1 tigre)|8|Si.
dama2|(hay habitacion2 dama)|8|Si.
dama2|(hay habitacion2 tigre)|8|No hay (mas) respuestas
dama3|(hay habitacion1 dama)|8|Si.
dama3|(hay habitacion1 tigre)|8|No hay (mas) respuestas
dama3|(hay habitacion2 dama)|8|Si.
dama3|(hay habitacion2 tigre)|8|No hay (mas) respuestas
dama4|(hay habitacion1 dama)|8|No hay (mas) respuestas
dama4|(hay habitacion1 tigre)|8|Si.
dama4|(hay habitacion2 dama)|8|Si.
dama4|(hay habitacion2 tigre)|8|No hay (mas) respuestas
dama6|(hay habitacion1 dama)|8|No hay (mas) respuestas
dama6|(hay habitacion1 tigre)|8|Si.
dama6|(hay habitacion2 dama)|8|Si.
dama6|(hay habitacion2 tigre)|8|No hay (mas) respuestas
dama5|(hay habitacion1 dama)|8|Si.
dama5|(hay habitacion1 tigre)|8|No hay (mas) respuestas
dama5|(hay habitacion2 dama)|8|No hay (mas) respuestas
dama5|(hay habitacion2 tigre)|8|Si.
dama5|(l1)|8|Si.
dama5|(l2)|8|Si.
dama7|(hay habitacion1 dama)|8|Si.
dama7|(hay habitacion1 tigre)|8|No hay (mas) respuestas
dama7|(hay habitacion2 dama)|8|No hay (mas) respuestas
dama7|(hay habitacion2 tigre)|8|Si.
dama7|(l1)|8|Si.
dama7|(l2)|8|Si.
presidente1|(presidente alfonsin)|8|No hay (mas) respuestas
presidente2|(presidente alfonsin)|8|Si.
presidente1|(presidente bustelo)|8|No hay (mas) respuestas
presidente2|(presidente bustelo)|8|No hay (mas) respuestas
presidente1|(presidente cafiero)|8|No hay (mas) respuestas
presidente2|(presidente cafiero)|8|No hay (mas) respuestas
transil1|(dice pepe)|8|No hay (mas) respuestas
transil1|(pepe es humano cuerdo)|8|No hay (mas) respuestas
transil1|(pepe es humano loco)|8|Si.
transil1|(pepe es vampiro cuerdo)|8|No hay (mas) respuestas
transil1|(pepe es vampiro loco)|8|No hay (mas) respuestas
transil2|(dice pepe)|8|No hay (mas) respuestas
transil2|(pepe es humano)|8|Si.
transil2|(pepe es vampiro)|8|No hay (mas) respuestas
transil2|(pepe esta cuerdo)|8|No hay (mas) respuestas
transil2|(pepe esta loco)|8|Si.
app|(append (cons a nil) (cons b nil) ?z)|8|?Z= [A B]
app|(append ?x ?y (cons a (cons b nil)))|8|?Y= [A B]
pert|(b pert (cons a (cons b nil)))|8|Si.
pert|(?x pert (cons a (cons b nil)))|8|?X= A
rev|(reverse (cons a (cons b nil)) ?r)|8|?R= [B A]
rev|(reverse (cons a (cons b (cons c nil))) ?r)|12|?R= [C B A]
app|(append [a] [b] ?z)|8|?Z= [A B]
rev|(reverse [a b c] ?r)|12|?R= [C B A]
CASOS
echo "PASS=$PASS  FAIL=$FAIL"
[ $FAIL -eq 0 ]
