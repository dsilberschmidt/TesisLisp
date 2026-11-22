
 ; inicial contiene: inicializacion, seth, on, on

(de inicializacion ()
   (inicializar-gensym)
   (setq nro-clausulas 0)

   (on  flag-control-tautologia)
   (on  flag-control-fusion)
   (on  flag-control-subsuncion)
   (on  flag-monitor)
   (on  flag-interrupcion)
   (on  flag-reciclaje)

   (setq llamada (eval ([soporte ancho] 1)))

   (setq admisibilidad-1 (eval ([sin-restriccion] 1)))
   (setq funcion-h-1 (eval ([constante menores unidades] 3)))
   (setq prioridad-1 (eval ([vertical horizontal] 2)))

   (setq admisibilidad-2
         (eval ([sin-restriccion entrada-lineal restriccion-fusion] 1))
   )
   (setq funcion-h-2 (eval ([constante menores unidades] 1)))
   (setq prioridad-2 (eval ([vertical horizontal] 2)))

   (seth)

   (setq limite-profundidad nil)

   (off   flag-filtrado-ancestros)
)

(de seth ()
   (setq heuristica-primaria
         (list admisibilidad-1 funcion-h-1 prioridad-1)
   )
   (setq heuristica-secundaria
         (list admisibilidad-2 funcion-h-2 prioridad-2)
   )
)

(df on (flags)
   (mapc (lambda (flag) (set flag t))
         flags
   )
)

(df off (flags)
   (mapc (lambda (flag) (set flag nil))
         flags
   )
)

(inicializacion)

(print 'inicial-ok)