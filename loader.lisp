;;; loader-orig.lisp — carga los fuentes ORIGINALES de 1989 (TESIS/, commit
;;; fca8f52 del 2020-05-10) SIN modificarlos. Todo el puente TLC-LISP -> SBCL
;;; vive en este archivo.
;;;
;;; Rasgos del dialecto que se emulan:
;;;  - Lector: \x es un caracter; [a b c] es un vector; | es un simbolo comun;
;;;    ^Z (fin de archivo DOS) es espacio.
;;;  - Lisp-1: las funciones son valores. (mapcar fusionar l), (setq h <),
;;;    (f x) con F variable, ([a b] 1) y ((store v i x) i) se traducen al
;;;    compilar cada DE con un recorredor de codigo (sb-walker).
;;;  - Vectores invocables con indice 1-based; NTH 1-based; = generico
;;;    (caracter y codigo de caracter son iguales); DO con salidas tipo COND;
;;;    IF con varios ELSE; DF = fexpr.

(defpackage :tlc
  (:use :cl)
  (:shadow #:if #:do #:= #:nth #:rest #:last #:union #:gensym #:listp
           #:string #:vector #:eval #:print #:read #:load))
(in-package :tlc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (sb-ext:muffle-conditions sb-ext:compiler-note style-warning warning)))

;;; ------------------------------------------------------------------ lector
(defvar *tlc-readtable* (copy-readtable nil))
(defun tlc-backslash (s c) (declare (ignore c)) (read-char s t nil t))
(defun tlc-bracket (s c) (declare (ignore c)) (coerce (read-delimited-list #\] s t) 'cl:vector))
(set-macro-character #\\ #'tlc-backslash t *tlc-readtable*)
(set-macro-character #\[ #'tlc-bracket nil *tlc-readtable*)
(set-macro-character #\] (get-macro-character #\) (copy-readtable nil)) nil *tlc-readtable*)
(set-syntax-from-char #\| #\a *tlc-readtable*)
(set-syntax-from-char (code-char 26) #\Space *tlc-readtable*)

;;; Los vectores se imprimen como en el dialecto: [A B C]
(set-pprint-dispatch '(and cl:vector (not cl:string))
  (lambda (s v) (format s "[~{~A~^ ~}]" (coerce v 'cl:list))))

;;; ------------------------------------------------------ primitivas del dialecto
(defun norm (x) (cl:if (characterp x) (char-code x) x))
(defun = (a b)                          ; igualdad generica; \x = su codigo
  (let ((a (norm a)) (b (norm b)))
    (or (eql a b) (and (numberp a) (numberp b) (cl:= a b)) (equal a b))))
(defun <> (a b) (not (= a b)))

(defmacro if (test then &rest else) `(cl:if ,test ,then (progn ,@else)))

(defun listp (x) (consp x))             ; NIL no es lista
(defun rest (l &optional (n 1)) (nthcdr n l))
(defun nth (x n)                        ; 1-based, listas, strings y vectores
  (etypecase x
    (cl:string (cl:if (<= 1 n (length x)) (char x (1- n)) nil))
    (cl:vector (aref x (1- n)))
    (cl:list (cl:nth (1- n) x))))
(defun add1 (x) (1+ x))
(defun sub1 (x) (1- x))
(defun append+ (&rest l) (apply #'append l))
(defun concat+ (&rest args) (apply #'list* args))
(defun eval (x)                         ; Lisp-1: el valor de un simbolo es su funcion
  (cl:if (and (symbolp x) (not (boundp x)) (fboundp x)) (fdefinition x) (cl:eval x)))

(defun pname (x) (etypecase x (symbol (symbol-name x)) (cl:string x)))
(defun insert (name) (intern name :tlc))
(defun string (&rest parts)
  (apply #'concatenate 'cl:string
         (mapcar (lambda (p) (typecase p
                               (cl:string p) (symbol (symbol-name p))
                               (character (cl:string p))
                               (integer (princ-to-string p))
                               (t (princ-to-string p))))
                 parts)))
(defun string-search (s ch)
  (let ((p (position (cl:if (characterp ch) ch (code-char ch)) s))) (and p (1+ p))))
(defun substring (s start end) (subseq s (1- start) end))

;; Vectores: VECTOR concatena (vectores se empalman, atomos se agregan)
(defun vector (&rest args)
  (coerce (loop for a in args
                append (cl:if (and (vectorp a) (not (stringp a))) (coerce a 'cl:list) (cl:list a)))
          'cl:vector))
(defun newvector (n x) (make-array n :initial-element x))
(defun memvec (x v) (let ((p (position x v :test #'=))) (and p (1+ p))))
(defun store (v i x) (setf (aref v (1- i)) x) v)

(defun pertenece (x l) (member x l :test #'=))
(defun newstring (n ch) (make-string n :initial-element (cl:if (characterp ch) ch (code-char ch))))

;; Invocacion Lisp-1: funcion, simbolo o vector (indice 1-based)
(defun tlc-call (f &rest args)
  (cond ((functionp f) (apply f args))
        ((symbolp f) (apply (fdefinition f) args))
        ((vectorp f) (aref f (1- (first args))))
        (t (error "TLC: no invocable: ~S" f))))

;; E/S
;; Convencion del dialecto: PRIN0/PRINT escriben con escapes, PRIN2/PRINT2
;; sin; las variantes PRINT* cortan la linea DESPUES (no antes como en CL):
;; (prin2 '?X) (prin2 '=) (print v) => "?X=[A B]" y salto de linea.
(defun print (x) (prin1 x) (terpri) x)
(defun prin0 (x) (prin1 x))
(defun prin2 (x) (princ x))
(defun print2 (x) (prog1 (princ x) (terpri)))
(defun print3 (x) (terpri) (princ x))
;; Antes de leer del teclado se vacia la salida: si no, el prompt ("?-",
;; "Interrupcion:", "Mas (s/n)?") aparece recien despues de lo que se tipea.
(defun read (&rest args) (finish-output) (apply #'cl:read args))
;; En modo por lotes (web) el teclado es una lista de teclas predefinidas;
;; agotada, responde "n". Asi ninguna pausa del original queda esperando.
(defvar *teclas* nil)
(defvar *modo-lotes* nil)
(defun console-in ()
  (when *modo-lotes*
    (return-from console-in (char-code (cl:if *teclas* (pop *teclas*) #\n))))
  (finish-output)
  (loop (let ((ch (read-char *standard-input* nil nil)))
          ;; Fin de entrada (uso no interactivo): terminar limpio. Los bucles
          ;; de teclado del original (Interrupcion, Mas s/n) no tienen salida
          ;; para una tecla que nunca llega.
          (cond ((null ch) (finish-output) (sb-ext:exit :code 0 :abort nil))
                ((not (member ch '(#\Newline #\Return))) (return (char-code ch)))))))

;;; ----------------------------------------------------------- formas especiales
(defmacro do (bindings exits &body body)  ; salidas: ((test res...) ...)
  `(cl:do ,bindings (nil)
     (cond ,@(mapcar (lambda (cl) (cl:if (cdr cl)
                                         `(,(car cl) (return (progn ,@(cdr cl))))
                                         `(,(car cl) (return ,(car cl)))))
                     exits))
     ,@body))
(defmacro rep (n &body body) (let ((i (cl:gensym))) `(dotimes (,i ,n) ,@body)))
(defmacro selectq (key &body clauses)
  (let ((k (cl:gensym)))
    `(let ((,k ,key))
       (cond ,@(mapcar (lambda (cl)
                         (destructuring-bind (keys &rest forms) cl
                           (let ((keys (cl:if (and (consp keys) (eq (car keys) 'quote)) (second keys) keys)))
                             (cond ((eq keys t) `(t ,@forms))
                                   ((consp keys) `((member ,k ',keys :test #'=) ,@forms))
                                   (t `((= ,k ',keys) ,@forms))))))
                       clauses)))))

;;; DE: traduce &OPT y aplica el recorredor Lisp-1. DF: fexpr.
(defvar *de-names* nil)                 ; funciones definidas con DE/DF en los fuentes

(defun lexical-p (sym env)
  (and env (sb-walker:var-lexical-p sym env)))

(defun lisp1-walk (form context env)
  (cond
    ((not (eq context :eval)) form)
    ;; simbolo usado como valor: si es nombre de funcion => #'f
    ((and (symbolp form) form (not (keywordp form)) (not (eq form t))
          (not (lexical-p form env)) (not (boundp form))
          (or (member form *de-names*)
              (and (fboundp form) (not (macro-function form)) (not (special-operator-p form)))))
     `(function ,form))
    ((atom form) form)
    (t (let ((f (car form)))
         (cond
           ((member f '(quote function)) form)
           ((and (consp f) (eq (car f) 'lambda)) form)
           ((or (consp f) (vectorp f)) `(tlc-call ,f ,@(cdr form)))
           ((and (symbolp f) (lexical-p f env)) `(tlc-call ,f ,@(cdr form)))
           ((and (symbolp f) (not (member f *de-names*))
                 (not (fboundp f)) (not (special-operator-p f)))
            `(tlc-call ,f ,@(cdr form)))
           (t form))))))

(defun fix-opt (args)
  (mapcar (lambda (a) (cl:if (and (symbolp a) (string= (symbol-name a) "&OPT")) '&optional a)) args))

(defmacro de (name args &body body &environment env)
  (pushnew name *de-names*)
  (let ((walked (sb-walker:walk-form `(lambda ,(fix-opt args) ,@body) env #'lisp1-walk)))
    `(progn (defun ,name ,@(cdr walked)) ',name)))

(defmacro df (name (flags) &body body)
  (let ((impl (intern (concatenate 'cl:string (symbol-name name) "%FEXPR"))))
    (pushnew impl *de-names*)
    `(progn (de ,impl (,flags) ,@body)
            (defmacro ,name (&rest args) (cl:list ',impl (cl:list 'quote args))))))
;; ON/OFF se usan (en INICIALIZACION) antes de su propio DF en INICIAL.LSP
(df on (flags) (mapc (lambda (f) (set f t)) flags))
(df off (flags) (mapc (lambda (f) (set f nil)) flags))

;;; Variables globales que el original usa antes de asignarlas
(defvar ? nil)
(dolist (v '(flag-control-tautologia flag-control-fusion flag-control-subsuncion
             flag-monitor flag-interrupcion flag-reciclaje flag-filtrado-ancestros
             limite-profundidad nro-clausulas heuristica-primaria heuristica-secundaria
             flag-mas supuestos-exitosos supuestos-fracasados base-interna goal-interno
             primer-resolvente llamada admisibilidad-1 admisibilidad-2 funcion-h-1
             funcion-h-2 prioridad-1 prioridad-2 vector-raices vector-numeros
             objeto i c monitor-c monitor-f))
  (proclaim `(special ,v))
  (unless (boundp v) (setf (symbol-value v) nil)))

;;; ------------------------------------------------------------------- carga
(defparameter *modulos*
  '("UTI" "UNIFICAR" "RENOMBRA" "UTICLA" "RESOLVER" "TAUTOLOG" "FUSION" "SUBSUME"
    "MONITOR" "RESPUES" "RECICLAR" "BUSQUEDA" "ESTRATEG" "CLAUSAL" "INICIAL"
    "EVALUAR" "INTERPRE"))

;; Pre-escaneo: nombres definidos con DE/DF (para referencias hacia adelante)
(let ((*readtable* *tlc-readtable*) (*package* (find-package :tlc)))
  (dolist (m *modulos*)
    (with-open-file (s (format nil "TESIS/~A.LSP" m))
      (loop for f = (handler-case (read s nil :eof) (reader-error () nil)) until (eq f :eof)
            when (and (consp f) (member (car f) '(de df)))
              do (pushnew (second f) *de-names*)))))

;; Carga forma por forma. El lector de TLC toleraba parentesis de cierre
;; sobrantes a nivel superior (hay uno en ESTRATEG.LSP, funcion ANCESTRO):
;; se ignoran y se reportan.
(defun read-tlc (s)
  (handler-case (read s nil :eof)
    (reader-error (e)
      (cl:if (search "unmatched close parenthesis" (princ-to-string e))
             (progn (format t "~&[TLC] parentesis sobrante ignorado en ~A~%" (pathname-name s))
                    (read-tlc s))
             (error e)))))
(defun load-tlc (path)
  (let ((*readtable* *tlc-readtable*) (*package* (find-package :tlc)))
    (with-open-file (s path)
      (handler-bind ((warning #'muffle-warning))
      (loop for f = (read-tlc s) until (eq f :eof)
            do (cl:eval (cl:if (and (consp f) (member (car f) '(de df)))
                               f
                               (sb-walker:walk-form f nil #'lisp1-walk))))))))
;; LOAD del dialecto: carga con el lector de TLC, sin ruido del compilador.
(defun load (path) (load-tlc path) t)
(dolist (m *modulos*) (load-tlc (format nil "TESIS/~A.LSP" m)))

(format t "~&TLC-ORIG cargado.~%")

;;; ---------------------------------------------------- modo por lotes (web)
;;; (consulta BASE META &key ...) corre una consulta completa sin interaccion
;;; y devuelve la salida del sistema de 1989 como texto.
;;;  BASE: nombre de una base de PROBLEMA/FUENTES.LSP (dama1, app...) o lista
;;;        de formulas.   META: la meta, ej. (append ?x ?y [a b]).
;;;  :respuestas N  cuantas respuestas pedir (default 5)
;;;  :limite N      limite de profundidad (default sin limite)
;;;  :monitor T     incluir la traza del monitor (sin pausas)
;;;  :anchura T     busqueda en anchura en vez de conjunto de soporte
;;;  :tiempo S      segundos maximos (default 20); al vencer devuelve lo
;;;                 obtenido hasta ahi. Pedir mas respuestas cuando ya no
;;;                 quedan puede no terminar nunca: la busqueda es completa.
;;; Nunca abre el Lisp anidado: las unicas teclas que ve son s/n.
(defvar *fuentes-cargadas* nil)
(defun fuentes ()
  (unless *fuentes-cargadas*
    (let ((*standard-output* (make-broadcast-stream)))
      (load-tlc "PROBLEMA/FUENTES.LSP"))
    (setf *fuentes-cargadas* t)))

(defun consulta (base meta &key (respuestas 5) limite monitor anchura (tiempo 20))
  (fuentes)
  (let* ((fuente (cond ((and (symbolp base) (boundp base)) (symbol-value base))
                       ((consp base) base)
                       (t (error "Base desconocida: ~A" base))))
         (*modo-lotes* t)
         (*teclas* (make-list (max 0 (1- respuestas)) :initial-element #\s))
         (salida (make-string-output-stream)))
    (handler-case
        (sb-ext:with-timeout tiempo
          (let ((*standard-output* salida))
            (set 'flag-monitor monitor)
            (set 'flag-interrupcion nil)
            (set 'limite-profundidad limite)
            (inicializar-gensym)          ; variables ?X_1... desde cero
            (let* ((objeto (compilar fuente 'objeto-web))
                   (bi (armar-clausulas objeto))
                   (gi (armar-goal meta)))
              (cl:if anchura (ancho bi gi) (soporte bi gi)))))
      (sb-ext:timeout ()
        (format salida "~&[tiempo agotado: ~D s]~%" tiempo)))
    (get-output-stream-string salida)))

;; Variante para entradas de texto (lo que llega de la web): lee con el lector
;; de TLC y SIN evaluacion en lectura (#. desactivado).
(defun consulta-texto (base-txt meta-txt &rest opciones)
  (let ((*readtable* *tlc-readtable*) (*package* (find-package :tlc)) (*read-eval* nil))
    (apply #'consulta (read-from-string base-txt) (read-from-string meta-txt) opciones)))

;; Entrada para el servidor web: base, meta y opciones llegan por variables de
;; entorno (sin comillas que escapar); la salida va tras una marca, para
;; separarla de los mensajes de carga.
(defun consulta-env ()
  (flet ((env (n &optional d) (or (sb-ext:posix-getenv n) d))
         (num (n d) (let ((v (sb-ext:posix-getenv n)))
                      (cl:if (and v (every #'digit-char-p v) (plusp (length v))) (parse-integer v) d))))
    (let ((res (handler-case
                   (consulta-texto (env "TLC_BASE" "nil") (env "TLC_META" "nil")
                                   :respuestas (min 20 (num "TLC_RESPUESTAS" 5))
                                   :limite (let ((l (num "TLC_LIMITE" 0))) (cl:if (plusp l) l nil))
                                   :monitor (equal (env "TLC_MONITOR") "1")
                                   :anchura (equal (env "TLC_ANCHURA") "1")
                                   :tiempo (min 25 (num "TLC_TIEMPO" 20)))
                 (error (e) (format nil "** error: ~A" e)))))
      (format t "~&=====SALIDA=====~%~A" res)
      (finish-output))))

;;; ------------------------------------------------------------- machete
(defparameter *ayuda* "
LGC - Lenguaje de Programacion Logica (tesis 1989)                MACHETE
------------------------------------------------------------------------
TOPLEVEL  >>>
  (load \"PROBLEMA/FUENTES.LSP\")  carga: dama1..dama7 presidente1/2
                                  transil1/2 app pert rev
  (c dama1)     compila una base        (i)      entra al interprete
  (ayuda)       este machete            (quit)   sale

INTERPRETE  ?-   (las metas van aca, no en el >>>)
  (hay habitacion2 dama)   (presidente ?x)   (append ?x ?y [a b])
  list          muestra la base         lisp     Lisp anidado
  add   (Enter) y despues la formula a agregar a la base
  on / off  (Enter) y despues el NOMBRE de UN flag (no una meta)
  exit          vuelve al toplevel

TECLAS  (escribir y Enter)
  Mas (s/n)?     s otra respuesta  n basta  r ver la prueba  l Lisp
  Interrupcion:  espacio sigue     l Lisp
  ancestros:     1 padre1  2 padre2  v bajar al hijo  q salir

FLAGS  (on por defecto, salvo la ultima)
  flag-monitor flag-interrupcion flag-control-tautologia
  flag-control-fusion flag-control-subsuncion flag-reciclaje
  flag-filtrado-ancestros   (tiene el bug de tipeo de 1989)
  En el >>>: (off flag-monitor flag-interrupcion)  apaga el monitor
            (off flag-interrupcion)               monitor sin pausas
  Atrapado en Interrupcion:  l Enter, (off flag-interrupcion), (quit)

ESTRATEGIA  (desde el >>>)
  (setq limite-profundidad 8)          sin limite por defecto
  (setq llamada ancho)                 anchura; defecto: soporte
  funcion-h-1: constante menores unidades*   prioridad-1: horizontal* vertical
  admisibilidad-2: sin-restriccion* entrada-lineal restriccion-fusion
  ej. (setq funcion-h-1 menores) y luego (seth)       (* = defecto)

BASES PROPIAS
  and or not -> <-> xor (paratodo (x) ...) (existe (x) ...)  listas [a b c]
  (setq mia '((paratodo (x) ((humano x) -> (mortal x))) (humano socrates)))
  (c mia) (i)  ?- (mortal ?quien)
")
(defun ayuda () (princ *ayuda*) (values))

;;; ------------------------------------------------------- REPL del dialecto
;;; (tlc::lgc) abre un toplevel como el de 1989 (prompt >>> como en la
;;; cartilla original): lee con el lector de TLC y
;;; evalua con semantica Lisp-1, de modo que (c dama1) y (i) funcionan como
;;; en la XT. (quit) o fin de entrada para salir.
(defun lgc ()
  (let ((*readtable* *tlc-readtable*) (*package* (find-package :tlc)))
    (format t "~&LGC 1989 - (ayuda) muestra el machete~%")
    (loop
      (format t "~&>>> ") (finish-output)
      (let ((f (read-tlc *standard-input*)))
        (when (or (eq f :eof) (equal f '(quit))) (return 'quit))
        (handler-case
            (let ((vs (handler-bind ((warning #'muffle-warning))
                        (multiple-value-list
                         (cl:eval (cl:if (and (consp f) (member (car f) '(de df)))
                                         f
                                         (sb-walker:walk-form f nil #'lisp1-walk)))))))
              (when vs (format t "~&~S" (car vs))))
          (error (e) (format t "~&** error: ~A" e)))))))
