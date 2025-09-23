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
      ;; Sombras por choques/semánticas de dialecto
      (:shadow rest last union gensym nth if list))))

;; 2) Cargar shim en :compat
(let ((*package* (find-package :compat)))
  (load "compat.lisp"))

;; 3) Entrar a :tesis
(in-package :tesis)

;; 3.1) Silenciar notas de compilador (ruido tipo "deleting unreachable code")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (sb-ext:muffle-conditions sb-ext:compiler-note)))

;; 4) Definir PRIMERO la macro IF (MacLisp-style):
;; (if TEST THEN . ELSE-FORMS)  => (cl:if TEST THEN (cl:progn ...ELSE-FORMS...))
(defmacro if (test then &rest else-forms)
  (cond
    ((null else-forms) `(cl:if ,test ,then))
    (t `(cl:if ,test ,then (cl:progn ,@else-forms)))))

;; 5) Prelude de compatibilidad (antes de compilar .LSP)

(defun rest (lst &optional (n 1)) (cl:nthcdr n lst))
(defun last (lst) (cl:if (cl:consp lst) (cl:car (cl:last lst)) nil))
(defun union (a b) (cl:union a b :test #'cl:equal))
(defun gensym (&optional (prefix "G")) (cl:gensym prefix))
(defun add1 (x) (cl:1+ x))
(defun sub1 (x) (cl:1- x))
(defun snoc (x lst) (cl:append lst (cl:list x)))
(defun pertenece (x lst) (cl:member x lst :test #'cl:equal))

;; MacLisp-style PNAME: lista de códigos ASCII del nombre del símbolo
(defun pname (x)
  (cl:map 'cl:list #'cl:char-code
          (etypecase x
            (symbol (cl:symbol-name x))
            (string x))))

;; MacLisp-style NTH: (nth lista n)  (orden invertido respecto a CL)
(defun nth (lst n)
  (etypecase lst
    (list (cl:nth n lst))
    (string (cl:char-code (cl:char lst n)))))

;; Vars heredadas que aparecen MUY temprano
(defparameter || nil)                 ;; símbolo de nombre vacío
(defparameter ?  (cl:char-code #\?))  ;; comparado con = en código legado

;; En código histórico: (MAP-2-AR LIST ...) pasa LIST “por nombre”.
;; Sombramos LIST y lo ligamos a la función CL:LIST para ese uso.
(defparameter list #'cl:list)

;; Macro DF (usada para ON/OFF de banderas en lote)
(defmacro df (fn (flags) &body _)
  (declare (ignore _))
  (let ((tmp (cl:gensym "FLAGS")))
    `(cl:let ((,tmp ,flags))
       (cl:mapc (cl:function
                 (cl:lambda (flag)
                   (cl:set flag ,(cl:cond
                                   ((cl:eq fn 'compat:on) t)
                                   ((cl:eq fn 'compat:off) nil)
                                   (t nil)))))
                ,tmp))))

;; 6) Compilar primero (detecta issues temprano)
(mapc #'cl:compile-file
      '("TESIS/UTI.LSP" "TESIS/UNIFICAR.LSP" "TESIS/RENOMBRA.LSP" "TESIS/RESOLVER.LSP"
        "TESIS/TAUTOLOG.LSP" "TESIS/FUSION.LSP" "TESIS/SUBSUME.LSP" "TESIS/MONITOR.LSP"
        "TESIS/ESTRATEG.LSP" "TESIS/RESPUES.LSP" "TESIS/BUSQUEDA.LSP" "TESIS/INICIAL.LSP"
        "TESIS/INTERPRE.LSP" "TESIS/UTICLA.LSP" "TESIS/CLAUSAL.LSP" "TESIS/RECICLAR.LSP"
        "TESIS/EVALUAR.LSP" "TESIS/LGC.LSP"))

;; 7) Cargar FASL
(mapc #'cl:load
      '("TESIS/UTI.fasl" "TESIS/UNIFICAR.fasl" "TESIS/RENOMBRA.fasl" "TESIS/RESOLVER.fasl"
        "TESIS/TAUTOLOG.fasl" "TESIS/FUSION.fasl" "TESIS/SUBSUME.fasl" "TESIS/MONITOR.fasl"
        "TESIS/ESTRATEG.fasl" "TESIS/RESPUES.fasl" "TESIS/BUSQUEDA.fasl" "TESIS/INICIAL.fasl"
        "TESIS/INTERPRE.fasl" "TESIS/UTICLA.fasl" "TESIS/CLAUSAL.fasl" "TESIS/RECICLAR.fasl"
        "TESIS/EVALUAR.fasl" "TESIS/LGC.fasl"))

(cl:format t "~&LGC cargado.~%")
