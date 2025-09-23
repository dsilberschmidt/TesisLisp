
 ; reciclar  contiene: reciclar, goal-instanciado, incluido

 ; uso de globales: primer-resolvente

(de reciclar (variables solucion &opt (hubo-reciclaje nil)
                                 &aux (supuesto (first solucion))
             )
   (if solucion
       (if (incluido supuesto supuestos-fracasados)
           (reciclar variables (rest solucion) hubo-reciclaje)
           (if (or (incluido supuesto supuestos-exitosos)
                   (evaluar base-interna (print (goal-instanciado supuesto)))
               )
               (progn (setq supuestos-exitosos
                            (cons supuesto supuestos-exitosos)
                      )
                      (pp-solucion variables supuesto)
                      (if (= (interactuar primer-resolvente) 'si)
                          (reciclar variables (rest solucion) 'true)
                          (setq flag-mas nil)
                          'true
                      )
               )
               (setq supuestos-fracasados
                     (cons supuesto supuestos-fracasados)
               )
               (reciclar variables (rest solucion) hubo-reciclaje)
           )
       )
       hubo-reciclaje
   )
)

; (de crear-base ()                              out
;   (append base-interna
;           (mappend goal-afirmado supuestos-exitosos)
;   )
;)

;(de goal-afirmado (supuesto)                    out
;)

(de goal-instanciado (supuesto
                        &aux (variables (first (third (first goal-interno))))
                     )
   (aplicar (map-2-ar #'list variables supuesto)
            (mapcar (lambda (x)(reemplazar x 3 nil))
                    goal-interno
            )
   )
)

(de incluido (literal literales)
   (if literales
       (or (<> (unificar=> (first literales) literal) 'fail)
           (incluido literal (rest literales))
       )
   )
)

(print 'reciclar-ok)
