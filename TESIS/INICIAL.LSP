
 ; inicial contiene: inicializacion, seth, on, on

(de seleccionar (indice &rest opciones)
   (cl:nth (sub1 indice) opciones)
)

(de inicializacion ()
   (inicializar-gensym)
   (setq nro-clausulas 0)

   (on  flag-control-tautologia)
   (on  flag-control-fusion)
   (on  flag-control-subsuncion)
   (on  flag-monitor)
   (on  flag-interrupcion)
   (on  flag-reciclaje)

   (setq llamada (seleccionar 1 #'soporte #'ancho))

   (setq admisibilidad-1 (seleccionar 1 #'sin-restriccion))
   (setq funcion-h-1 (seleccionar 3 #'constante #'menores #'unidades))
   (setq prioridad-1 (seleccionar 2 #'vertical #'horizontal))

   (setq admisibilidad-2
         (seleccionar 1 #'sin-restriccion #'entrada-lineal #'restriccion-fusion)
   )
   (setq funcion-h-2 (seleccionar 1 #'constante #'menores #'unidades))
   (setq prioridad-2 (seleccionar 2 #'vertical #'horizontal))

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

(print 'inicial-ok)
