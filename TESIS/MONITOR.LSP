
 ; monitor contiene: monitor, prin-elementos, pp-clausula, pp-clausulas,
 ;                   recorrer-ancestros, interrupcion, shell

 ; (load "uti")      necesita: posicion

(de monitor (expandidas expandibles)
   (if flag-monitor
       (progn (print "expandidas")
              (pp-clausulas expandidas)
              (terpri)
              (print "expandibles")
              (pp-clausulas expandibles)
              (terpri)
       )
   )
)

(de prin-elementos (lista)
   (if lista
       (progn (prin2 (first lista))
              (prin-elementos (rest lista))
       )
   )
)

(de pp-clausula (clausula &opt (columna 0) &rest opcion)
   (prin2 (newstring columna \ ))
   (prin-elementos (list (sixth clausula) \:))
   (prin-elementos (first clausula))
   (prin2  '<- )
   (prin-elementos (second clausula))
   (if (pertenece 'v opcion)
       (progn (prin2 "       [")
              (prin-elementos (third clausula))
              (prin2 \])
       )
   )
   (terpri)
)

(de pp-clausulas (clausulas)
   (mapc (lambda (x) (pp-clausula x 9 'v)) clausulas)
   clausulas
)

(de recorrer-ancestros (clausula)
   (if clausula
       (progn
           (terpri) (pp-clausula (fourth clausula) 0 'v)
           (terpri) (pp-clausula (fifth clausula) 0 'v)
           (terpri) (pp-clausula clausula 9 'v)
           (setq tecla
                 (loop
                    (prin2 "Opcion (1, 2, V, Q):")
                    (let ((char (console-in)))
                       (when (pertenece char (list |1| |2| |v| |q|))
                          (print char)
                          (return char))
                    )
                 )
           )
           (if (selectq tecla
                    (\1      (recorrer-ancestros (fourth clausula)))
                    (\2      (recorrer-ancestros (fifth clausula)))
                    ((\v \q) nil)
               )
               (recorrer-ancestros clausula)
               (= tecla \v)
           )
       )
       (print "Clausula base.")
   )
)

(de interrupcion ()
   (if flag-interrupcion
       (loop
          (prin2 "Interrupcion:")
          (let ((char (console-in)))
             (when (= char | |)
                (print2 "cont")
                (return))
             (when (<> (shell char) 'nada)
                nil))
       )
   )
)

(de shell (char &opt (clausula nil))
   (cond
      ( (= char \l) (lisp))
      ( (and (= char \r) clausula) (recorrer-ancestros clausula))
      ( t 'nada)
   )
)

(print 'monitor-ok)
