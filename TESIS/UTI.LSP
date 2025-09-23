
 ; uti contiene: cabeza, last, posicion, rotar, snoc, reemplazar,
 ;               separar, implicar, armar-clausula, numerar,
 ;               map-2-ar, union, interseccion, compactar,
 ;               compactar-orden, lisp, quit

(de cabeza (lista numero)
   (if (and (> numero 0) lista)
       (cons (first lista) (cabeza (rest lista) (sub1 numero)))
   )
)

(de last (lista)
   (first (reverse lista))
)

(de posicion (elemento lista &opt (lugar 1))
   (if lista
      (if (= (first lista) elemento)
          lugar
          (posicion elemento (rest lista) (add1 lugar))
      )
   )
)

(de rotar (lista)
   (snoc (first lista) (rest lista))
)

(de snoc (expr lista)
   (append lista (list expr))
)

(de reemplazar (lista numero expresion)
   (append+ (cabeza lista (sub1 numero))
            (list expresion)
            (rest lista numero)
   )
)

(de separar (implicacion &opt (positivos nil))
   (if (and (<> (first implicacion) '<-) implicacion)
       (separar (rest implicacion) (snoc (first implicacion) positivos))
       (list positivos (rest implicacion))
   )
)

(de implicar (clausulas)
   (mapcar (lambda (x) (append (first x) (cons '<- (second x))))
           clausulas
   )
)

(de armar-clausula (clausula &opt (padre1 nil) (padre2 nil)
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

(de numerar ()
   (setq nro-clausulas (add1 nro-clausulas))
)

(de map-2-ar (funcion lista1 lista2)
   (if (and lista1 lista2)
       (cons (funcion (first lista1) (first lista2))
             (map-2-ar funcion (rest lista1) (rest lista2))
       )
   )
)

(de mappend (funcion lista)
   (if lista
       (append (funcion (first lista))
               (mappend funcion (rest lista))
       )
   )
)

(de union (lista1 lista2)
   (compactar-orden (append lista1 lista2))
)

(de interseccion (conjunto1 conjunto2)
   (if conjunto1
      (if (pertenece (first conjunto1) conjunto2)
          (cons (first conjunto1) (interseccion (rest conjunto1) conjunto2))
          (interseccion (rest conjunto1) conjunto2)
      )
   )
)

(de compactar (lista)
   (if lista
       (if (pertenece (first lista) (rest lista))
           (compactar (rest lista))
           (cons (first lista) (compactar (rest lista)))
       )
   )
)

(de compactar-orden (lista)
   (reverse (compactar (reverse lista)))
)

(de lisp ()
   (prin2 '>>>)
   (if (<> (print (eval (read))) 'quit)
       (lisp)
   )
)

(de quit () 'quit)

(print 'uti-ok)