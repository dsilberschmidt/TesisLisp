
 ; resolver contiene:  resolver-diagonal, resolver-completo, nueva-prof

 ; (load "unificar")   necesita: unificar, aplicar

 ; (load "uti")        necesita: snoc, armar-clausula

 ; (load "renombra")   necesita: renombrar

(de resolver-diagonal (pos1 neg1 pos2 neg2 respuestas
                       &opt (aux-pos1 nil) (aux-neg2 nil) &aux unif
                      )
  (if pos1
    (if neg2
      (if (<> (setq unif (unificar (first pos1) (first neg2))) 'fail)
          (cons (renombrar (aplicar unif
                                    (list (append+ aux-pos1 (rest pos1) pos2)
                                          (append+ neg1 aux-neg2 (rest neg2))
                                          respuestas
                                    )
                       )
                )
                (resolver-diagonal pos1 neg1 pos2 (rest neg2) respuestas
                                   aux-pos1 (snoc (first neg2) aux-neg2)
                )
          )
          (resolver-diagonal pos1 neg1 pos2 (rest neg2) respuestas
                             aux-pos1 (snoc (first neg2) aux-neg2)
          )
      )
      (resolver-diagonal (rest pos1) neg1 pos2 aux-neg2 respuestas
                         (snoc (first pos1) aux-pos1) nil
      )
    )
  )
)

(de resolver-completo (clausula1 clausula2
                       &aux (profundidad (nueva-prof clausula1 clausula2))
                      )
  (mapcar (lambda (x) (armar-clausula x clausula1 clausula2 profundidad))
          (append (resolver-diagonal
                       (first clausula1) (second clausula1)
                       (first clausula2) (second clausula2)
                       (union (third clausula1) (third clausula2))
                  )
                  (resolver-diagonal
                       (first clausula2) (second clausula2)
                       (first clausula1) (second clausula1)
                       (union (third clausula1) (third clausula2))
                  )
          )
  )
)

(de nueva-prof (clausula1 clausula2)
   (add1 (max (sixth clausula1) (sixth clausula2)))
)

(print 'resolver-ok)