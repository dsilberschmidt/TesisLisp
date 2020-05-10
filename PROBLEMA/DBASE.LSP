
 ; dbase

(setq base-append '( ( (append nil ?x ?x) )
                     ( (append (cons ?w ?x) ?y (cons ?w ?z))
                           <- (append ?x ?y ?z)
                     )
                   )
)

(setq base-reverse '( ( (append nil ?x ?x) )
                      ( (append (cons ?w ?x) ?y (cons ?w ?z))
                                   <- (append ?x ?y ?z)
                      )
                      ( (reverse nil nil) )
                      ( (reverse (cons ?h ?t) ?l)
                                   <- (reverse ?t ?z)
                                      (append ?z (cons ?h nil) ?l)
                      )
                    )
)

(print 'dbase-ok)