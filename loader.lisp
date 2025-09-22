;;; loader.lisp

;; 1) Paquetes
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :compat)
    (defpackage :compat
      (:use :cl)
      (:export de on off <> sub1 restn append+ prin2 print2 selectq)))
  (unless (find-package :tesis)
    (defpackage :tesis
      (:use :cl :compat)
      ;; Sombras por choques con CL
      (:shadow rest last union gensym))))

;; 2) Cargar shim en :compat
(let ((*package* (find-package :compat)))
  (load "compat.lisp"))

;; 3) Entrar a :tesis
(in-package :tesis)

;; 4) Prelude de compatibilidad en :tesis (antes de compilar .LSP)
;;    Define las funciones que el código antiguo espera en este paquete.
(defun rest (lst &optional (n 1)) (nthcdr n lst))
(defun last (lst) (if (consp lst) (car (cl:last lst)) nil))
(defun union (a b) (cl:union a b :test #'equal))
(defun gensym (&optional (prefix "G")) (cl:gensym prefix))
(defun add1 (x) (1+ x))
(defun snoc (x lst) (append lst (list x)))
(defun pertenece (x lst) (member x lst :test #'equal))
(defun pname (x) (if (symbolp x) (symbol-name x) (error "PNAME: ~S no es símbolo" x)))

;; 5) Compilar primero (detecta issues temprano)
(mapc #'compile-file
      '("TESIS/UTI.LSP" "TESIS/UNIFICAR.LSP" "TESIS/RENOMBRA.LSP" "TESIS/RESOLVER.LSP"
        "TESIS/TAUTOLOG.LSP" "TESIS/FUSION.LSP" "TESIS/SUBSUME.LSP" "TESIS/MONITOR.LSP"
        "TESIS/ESTRATEG.LSP" "TESIS/RESPUES.LSP" "TESIS/BUSQUEDA.LSP" "TESIS/INICIAL.LSP"
        "TESIS/INTERPRE.LSP" "TESIS/UTICLA.LSP" "TESIS/CLAUSAL.LSP" "TESIS/RECICLAR.LSP"
        "TESIS/EVALUAR.LSP" "TESIS/LGC.LSP"))

;; 6) Cargar FASL
(mapc #'load
      '("TESIS/UTI.fasl" "TESIS/UNIFICAR.fasl" "TESIS/RENOMBRA.fasl" "TESIS/RESOLVER.fasl"
        "TESIS/TAUTOLOG.fasl" "TESIS/FUSION.fasl" "TESIS/SUBSUME.fasl" "TESIS/MONITOR.fasl"
        "TESIS/ESTRATEG.fasl" "TESIS/RESPUES.fasl" "TESIS/BUSQUEDA.fasl" "TESIS/INICIAL.fasl"
        "TESIS/INTERPRE.fasl" "TESIS/UTICLA.fasl" "TESIS/CLAUSAL.fasl" "TESIS/RECICLAR.fasl"
        "TESIS/EVALUAR.fasl" "TESIS/LGC.fasl"))

(format t "~&LGC cargado.~%")
