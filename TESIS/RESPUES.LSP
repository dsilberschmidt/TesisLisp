
 ; respuesta contiene: eliminar-respuestas-y-mostrarlas, clausula-no-nula,
 ;                     interactuar, mostrar-solucion, pp-solucion

(de eliminar-respuestas-y-mostrarlas
         (resolventes raices &aux (primer-resolvente (first resolventes))
                                  (solucion (third primer-resolvente))
         )                         ; solucion es una posible solucion
  (if resolventes
     (if (clausula-no-nula primer-resolvente)
        (cons primer-resolvente
              (eliminar-respuestas-y-mostrarlas (rest resolventes) raices)
        )
        (if (and (> (length solucion) 1)
                 flag-reciclaje
                 raices                      ; sacable por union
                 (reciclar raices solucion)
            )
            (if flag-mas
                (eliminar-respuestas-y-mostrarlas (rest resolventes) raices)
            )
            (if (= (length solucion) 1)
                (setq supuestos-exitosos
                      (cons (first solucion) supuestos-exitosos)
                )
            )
            (mostrar-solucion raices solucion)
            (if (eq 'si (interactuar primer-resolvente))
                (eliminar-respuestas-y-mostrarlas (rest resolventes) raices)
                (setq flag-mas nil)
            )
        )
     )
  )
)

(de interactuar (&opt (clausula nil))
   (prin2 "Mas (s/n)?")
   (let ((char (console-in)))
      (loop
         (cond
            ((= char |s|)
             (print 'si)
             (return 'si))
            ((= char |n|)
             (print 'no)
             (return 'no))
            (t (if (<> (shell char clausula) 'nada)
                   (prin2 "Mas (s/n)?")
               )
               (setq char (console-in))
            )
         )
      )
   )
)

(de mostrar-solucion (variables solucion)
   (if (null variables)
       (print2 "Si.")
       (pp-solucion variables (first solucion))
       (if (rest solucion)
           (progn (print2 "O bien.")
                  (mostrar-solucion variables (rest solucion))
           )
       )
   )
)

(de pp-solucion (variables solucion)
   (map-2-ar (lambda (x y) (prin2 x) (prin2 '=) (print y))
             variables
             (ltov solucion)
   )
)

(de clausula-no-nula (clausula)
   (or (first clausula) (second clausula))
)

(print 'respuesta-ok)
