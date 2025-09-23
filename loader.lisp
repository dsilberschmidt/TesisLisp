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
      (:shadow rest last union gensym nth if list string vector))))

;; 2) Cargar shim en :compat
(let ((*package* (find-package :compat)))
  (load "compat.lisp"))

;; 3) Entrar a :tesis
(in-package :tesis)

;; 3.1) Silenciar notas del compilador (ruido)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (sb-ext:muffle-conditions sb-ext:compiler-note)))

;; 4) Macro IF (MacLisp): (if TEST THEN . ELSE-FORMS)
(defmacro if (test then &rest else-forms)
  (cl:if else-forms
         `(cl:if ,test ,then (cl:progn ,@else-forms))
         `(cl:if ,test ,then)))

;; 5) Prelude de compatibilidad (antes de compilar .LSP)
(defun rest (lst &optional (n 1)) (cl:nthcdr n lst))
(defun last (lst) (cl:if (cl:consp lst) (cl:car (cl:last lst)) nil))
(defun union (a b) (cl:union a b :test #'cl:equal))
(defun gensym (&optional (prefix "G")) (cl:gensym prefix))
(defun add1 (x) (cl:1+ x))
(defun sub1 (x) (cl:1- x))
(defun snoc (x lst) (cl:append lst (cl:list x)))
(defun pertenece (x lst) (cl:member x lst :test #'cl:equal))

;; PNAME (lista de códigos)
(defun pname (x)
  (cl:map 'cl:string #'cl:char-code
          (etypecase x
            (symbol (cl:symbol-name x))
            (string x))))

;; NTH estilo MacLisp: (nth lista n)
(defun nth (lst n)
  (etypecase lst
    (list (cl:nth n lst))
    (string (cl:char-code (cl:char lst n)))))

;; Vars heredadas que aparecen MUY temprano
(defparameter || nil)
(defparameter ?  (cl:char-code #\?))

;; LIST “por nombre” en (MAP-2-AR LIST ...)
(defparameter list #'cl:list)

;; STRING del dialecto: concatena símbolos/strings/char-codes/listas de codes/etc.
(defun string (&rest parts)
  (let ((pieces
          (mapcar (lambda (p)
                    (cond
                      ((cl:stringp p) p)
                      ((cl:symbolp p) (cl:symbol-name p))
                      ((cl:characterp p) (cl:string p))
                      ((cl:integerp p) (cl:string (cl:code-char p)))
                      ((and (cl:listp p) (every #'cl:integerp p))
                       (cl:map 'cl:string #'cl:code-char (coerce p 'vector)))
                      (t (princ-to-string p))))
                  parts)))
    (apply #'cl:concatenate 'cl:string pieces)))

;; VECTOR del dialecto: si primer arg es vector, devuelve vector extendido
(defun vector (&rest args)
  (if (and args (cl:vectorp (first args)))
      (let ((vec (first args))
            (more (rest args)))
        (cl:concatenate 'vector vec (coerce more 'vector)))
      (apply #'cl:vector args)))

;; Emulación array-funcall (MacLisp):
(defvar vector-raices (make-array 0 :element-type t))
(defvar vector-numeros (make-array 0 :element-type t))

(defmacro vector-raices (i) `(cl:aref vector-raices ,i))
(defmacro vector-numeros (i) `(cl:aref vector-numeros ,i))

(defun memvec (item vec) (position item vec :test #'cl:equal))
(defun store (vec i val)
  (setf (cl:aref vec i) val)
  ;; para soportar ((STORE ...) POSICION) => POSICION
  (cl:lambda (x) x))

(defun insert (name)
  ;; Inserta símbolo (intern) a partir del string NAME en el paquete :TESIS
  (cl:intern name (find-package :tesis)))

;; Macro DF (ON/OFF de banderas)
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
