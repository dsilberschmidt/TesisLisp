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

;; 4) Prelude de compatibilidad (antes de compilar .LSP)

;; ---- A. PRIMITIVAS Y UTILIDADES ----
(defun rest (lst &optional (n 1)) (nthcdr n lst))
(defun last (lst) (if (consp lst) (car (cl:last lst)) nil))
(defun union (a b) (cl:union a b :test #'equal))
(defun gensym (&optional (prefix "G")) (cl:gensym prefix))
(defun add1 (x) (1+ x))
(defun sub1 (x) (1- x))
(defun snoc (x lst) (append lst (list x)))
(defun pertenece (x lst) (member x lst :test #'equal))

;; MacLisp-style PNAME: lista de códigos ASCII del nombre del símbolo
(defun pname (x)
  (map 'list #'char-code
       (etypecase x
         (symbol (symbol-name x))
         (string x))))

;; MacLisp-style NTH: (nth lista n)  (orden invertido respecto a CL)
(defun nth (lst n)
  (etypecase lst
    (list (cl:nth n lst))
    (string (char-code (char lst n)))))

;; ---- B. VARS HEREDADAS QUE SE USAN EN TOPLEVEL ----
;; Importante: ligar || ANTES de compilar/cargar cualquier .LSP
(defparameter || nil)          ;; símbolo de nombre vacío
(defparameter ?  (char-code #\?))

;; En el código histórico aparecen llamadas como (MAP-2-AR LIST ...).
;; Sombramos LIST y lo ligamos a la función CL:LIST para ese uso.
(defparameter list #'cl:list)

;; ---- C. IF multi-rama (estilo MacLisp): (if test1 then1 test2 then2 ... [else])
(defmacro if (test then &rest more)
  (cond
    ((null more) `(cl:if ,test ,then))
    ((null (cdr more)) `(cl:if ,test ,then ,(car more)))
    (t ;; más de 1 par -> cond anidado
     (let ((pairs '()) (else-form nil) (rest more)))
       ;; partir en pares (test then) y opcional else final
       (loop while rest do
         (let ((a (pop rest)))
           (if rest
               (let ((b (pop rest))) (push (list a b) pairs))
               (setf else-form a))))
       (setf pairs (nreverse pairs))
       `(cond
          (,test ,then)
          ,@pairs
          ,@(when else-form `((t ,else-form))))))))

;; ---- D. Macro DF (usada para ON/OFF en listas de flags)
(defmacro df (fn (flags) &body body)
  (declare (ignore body))
  (let ((tmp (gensym "FLAGS")))
    `(let ((,tmp ,flags))
       (mapc (lambda (flag)
               (set flag ,(cond
                            ((eq fn 'compat:on) t)
                            ((eq fn 'compat:off) nil)
                            (t nil))))
             ,tmp))))

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
