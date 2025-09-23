
 ; interprete contiene: interprete, compilar, armar-clausulas, armar-goal
 ;                      add, i, c

(de interprete (&opt (base-objeto objeto)
                     (goal-fuente (progn (prin2 '?-) (read)))
                &aux (base-interna (armar-clausulas base-objeto))
                     (goal-interno (armar-goal goal-fuente))
                     (supuestos-exitosos nil)
                     (supuestos-fracasados nil)
               )
   (selectq goal-fuente
       ( 'exit   nil)
       ( 'lisp   (print2 "Use (quit) para retornar al sistema.")
                 (lisp)  (interprete base-objeto)
       )
       ( 'add    (interprete (add base-objeto (read))))
       ( 'list   (pp-clausulas base-interna)
                 (interprete base-objeto)
       )
       ( 'on     (eval `(on  ,(read))) (interprete base-objeto))
       ( 'off    (eval `(off ,(read))) (interprete base-objeto))
       ( t       (funcall llamada base-interna goal-interno)
                (interprete base-objeto)
       )
   )
)

(de compilar (base-fuente &opt (base-objeto 'objeto))
   (set base-objeto
        (implicar (clausal base-fuente))
   )
)


(de armar-clausulas (implicaciones)
   (ordenar
        (normalizar (mapcar (lambda (implicacion)
                                    (armar-clausula (separar implicacion))
                            )
                            (vtol implicaciones)
                    )
        )
        nil
        heuristica-secundaria
   )
)

(de armar-goal (goal-fuente
                 &aux (goal (clausal (list (negar goal-fuente))))
                      (variables (sacar-anonimas (hallar-variables goal)))
               )
   (ordenar
        (normalizar (mapcar #'armar-clausula
                            (mapcar (lambda (clausula)
                                            (snoc (list variables) clausula)
                                    )
                                    (vtol goal)
                            )
                    )
        )
        nil
        heuristica-primaria
   )
)

(de add (base formula)
   (append base (implicar (clausal (list formula))))
)

(setq i #'interprete c #'compilar)

(print 'interprete-ok)
