
 ; uti contiene: cabeza, last, posicion, rotar, snoc, reemplazar,
 ;               separar, implicar, armar-clausula, numerar,
 ;               map-2-ar, union, interseccion, compactar,
 ;               compactar-orden, lisp, quit

(defun cabeza (lista numero)
   (if (and (> numero 0) lista)
       (cons (first lista) (cabeza (rest lista) (sub1 numero)))
   )
)

(defun last (lista)
   (first (reverse lista))
)

(defun posicion (elemento lista &opt (lugar 1))
   (if lista
      (if (= (first lista) elemento)
          lugar
          (posicion elemento (rest lista) (add1 lugar))
      )
   )
)

(defun rotar (lista)
   (snoc (first lista) (rest lista))
)

(defun snoc (expr lista)
   (append lista (list expr))
)

(defun reemplazar (lista numero expresion)
   (append+ (cabeza lista (sub1 numero))
            (list expresion)
            (rest lista numero)
   )
)

(defun separar (implicacion &opt (positivos nil))
   (if (and (<> (first implicacion) '<-) implicacion)
       (separar (rest implicacion) (snoc (first implicacion) positivos))
       (list positivos (rest implicacion))
   )
)

(defun implicar (clausulas)
   (mapcar (lambda (x) (append (first x) (cons '<- (second x))))
           clausulas
   )
)

(defun armar-clausula (clausula &opt (padre1 nil) (padre2 nil)
                                  (profundidad 0)
                             &aux (positivos (first clausula))
                                  (negativos (second clausula))
                                  (respuestas (third clausula))
                                  (tipo 'clausula)
                                  (numero (numerar))
                   )
   (list positivos negativos respuestas
         padre1 padre2 profundidad tipo numero
   )
)

(defun numerar ()
   (setq nro-clausulas (add1 nro-clausulas))
)

(defun map-2-ar (funcion lista1 lista2)
   (if (and lista1 lista2)
       (cons (funcion (first lista1) (first lista2))
             (map-2-ar funcion (rest lista1) (rest lista2))
       )
   )
)

(defun mappend (funcion lista)
   (if lista
       (append (funcion (first lista))
               (mappend funcion (rest lista))
       )
   )
)

(defun union (lista1 lista2)
   (compactar-orden (append lista1 lista2))
)

(defun interseccion (conjunto1 conjunto2)
   (if conjunto1
      (if (pertenece (first conjunto1) conjunto2)
          (cons (first conjunto1) (interseccion (rest conjunto1) conjunto2))
          (interseccion (rest conjunto1) conjunto2)
      )
   )
)

(defun compactar (lista)
   (if lista
       (if (pertenece (first lista) (rest lista))
           (compactar (rest lista))
           (cons (first lista) (compactar (rest lista)))
       )
   )
)

(defun compactar-orden (lista)
   (reverse (compactar (reverse lista)))
)

(defun lisp ()
   (prin2 '>>>)
   (if (<> (print (eval (read))) 'quit)
       (lisp)
   )
)

(defun quit () 'quit)

(print 'uti-ok)
