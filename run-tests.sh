#!/usr/bin/env bash
# run-tests.sh — regresion batch para TesisLisp-sbcl
# Uso: ./run-tests.sh
# Cada test corre SBCL no-interactivo (monitor e interrupcion apagados),
# con timeout, y verifica que la salida contenga "Si." (demostrado).
# dama1: A xor (A xor B) == B  =>  dama en hab2, tigre en hab1.
# "(hay habitacion1 dama)" NO es demostrable: se espera "No hay (mas) respuestas".

set -u
cd "$(dirname "$0")"
TIMEOUT_SECS=${TIMEOUT_SECS:-45}
PASS=0; FAIL=0; KNOWN=0

run_case () {
  local name="$1" base="$2" goal="$3" limite="$4" expect="$5" known_fail="$6"
  local out
  out=$(echo n | timeout "$TIMEOUT_SECS" sbcl --non-interactive \
      --load loader.lisp \
      --eval "(in-package :tesis)" \
      --eval "(off flag-monitor)" \
      --eval "(off flag-interrupcion)" \
      --load PROBLEMA/FUENTES.LSP \
      --eval "(compilar $base 'obj-test)" \
      --eval "(setq base-interna (armar-clausulas obj-test))" \
      --eval "(setq goal-interno (armar-goal '$goal))" \
      --eval "(setq limite-profundidad $limite)" \
      --eval "(soporte base-interna goal-interno)" 2>&1)
  local rc=$?
  if [ $rc -eq 124 ]; then
    verdict="TIMEOUT (${TIMEOUT_SECS}s)"
  elif echo "$out" | grep -q "$expect"; then
    verdict="OK"
  else
    verdict="SIN-RESPUESTA (rc=$rc)"
  fi
  if [ "$verdict" = "OK" ]; then
    echo "PASS        $name"
    PASS=$((PASS+1))
  elif [ "$known_fail" = "yes" ]; then
    echo "KNOWN-FAIL  $name  [$verdict]  (explosion de frontera, ver TODO.md)"
    KNOWN=$((KNOWN+1))
  else
    echo "FAIL        $name  [$verdict]"
    echo "$out" | tail -15 | sed 's/^/            | /'
    FAIL=$((FAIL+1))
  fi
}

echo "== TesisLisp regresion =="
#         nombre                base         goal                      limite  espera  known-fail
run_case  "presidente2/alfonsin" presidente2 "(presidente alfonsin)"   10      "Si."   no
run_case  "dama1/hab2-dama"      dama1       "(hay habitacion2 dama)"  8       "Si."   no
run_case  "dama1/hab1-tigre"     dama1       "(hay habitacion1 tigre)" 8       "Si."   no
run_case  "dama1/NO-hab1-dama"   dama1       "(hay habitacion1 dama)"  8       "No hay (mas) respuestas"   no
run_case  "dama2/hab1-tigre"     dama2       "(hay habitacion1 tigre)" 8       "Si."   no

echo "=========================="
echo "PASS=$PASS  FAIL=$FAIL  KNOWN-FAIL=$KNOWN"
# exit code: solo FAIL inesperados rompen la corrida
[ $FAIL -eq 0 ]
