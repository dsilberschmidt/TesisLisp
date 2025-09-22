;;; loader.lisp

;; 1) Definir paquetes
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :compat)
    (defpackage :compat
      (:use :cl)
      (:export de on off <> sub1 restn append+ prin2 print2 selectq)))
  (unless (find-package :tesis)
    (defpackage :tesis
      (:use :cl :compat)
      (:shadow rest last union))))   ; sombras conflictivas

;; 2) Cargar el shim en :compat
(let ((*package* (find-package :compat)))
  (load "compat.lisp"))

;; 3) Trabajar desde el paquete :tesis
(in-package :tesis)

;; 4) Compilar primero (detecta issues temprano)
(mapc #'compile-file
      '("TESIS/UTI.LSP" "TESIS/UNIFICAR.LSP" "TESIS/RENOMBRA.LSP" "TESIS/RESOLVER.LSP"
        "TESIS/TAUTOLOG.LSP" "TESIS/FUSION.LSP" "TESIS/SUBSUME.LSP" "TESIS/MONITOR.LSP"
        "TESIS/ESTRATEG.LSP" "TESIS/RESPUES.LSP" "TESIS/BUSQUEDA.LSP" "TESIS/INICIAL.LSP"
        "TESIS/INTERPRE.LSP" "TESIS/UTICLA.LSP" "TESIS/CLAUSAL.LSP" "TESIS/RECICLAR.LSP"
        "TESIS/EVALUAR.LSP" "TESIS/LGC.LSP"))

;; 5) Cargar los FASL
(mapc #'load
      '("TESIS/UTI.fasl" "TESIS/UNIFICAR.fasl" "TESIS/RENOMBRA.fasl" "TESIS/RESOLVER.fasl"
        "TESIS/TAUTOLOG.fasl" "TESIS/FUSION.fasl" "TESIS/SUBSUME.fasl" "TESIS/MONITOR.fasl"
        "TESIS/ESTRATEG.fasl" "TESIS/RESPUES.fasl" "TESIS/BUSQUEDA.fasl" "TESIS/INICIAL.fasl"
        "TESIS/INTERPRE.fasl" "TESIS/UTICLA.fasl" "TESIS/CLAUSAL.fasl" "TESIS/RECICLAR.fasl"
        "TESIS/EVALUAR.fasl" "TESIS/LGC.fasl"))

(format t "~&LGC cargado.~%")
