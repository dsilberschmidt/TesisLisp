
 ; subsume contiene:   subsume-literal, subsume-literales, unificar=>
 ;                     subsume, subsumida, eliminar-subsumidas
 ;                     auto-depurar-subsumidas

 ; (load "unificar")   necesita: variablep, ocurre, aplicar, componer

 ; (load "uti")        necesita: rotar

(de unificar=> (literal1 literal2
                &aux unif (variables (hallar-variables literal2))
                     (constantes (mapcar (lambda (x) (gensym 'k)) variables))
                     (unif1 (map-2-ar list variables constantes))
                     (unif2 (map-2-ar list constantes variables))
               )
   (if (<> (setq unif (unificar literal1 (aplicar unif1 literal2))) 'fail)
       (sustituir unif2 unif)
       'fail
   )
)

(de subsume-literal (literal literales &aux unif)
   (if literales
      (if (<> (setq unif (unificar literal (first literales))) 'fail)
          unif
          (subsume-literal literal (rest literales))
      )
      'fail
   )
)

(de subsume-literales (lit1 lit2 &aux unif1 unif2)       ; lit = literales
   (if lit1
      (if (and (<> 'fail (setq unif1 (subsume-literal (first lit1) lit2)))
               (<> 'fail
                   (setq unif2
                         (subsume-literales (aplicar unif1 (rest lit1)) lit2)
                   )
               )
          )
          (componer unif2 unif1)
          'fail
      )
   )
)

(de subsume (clausula1 clausula2
             &aux unif (variables (hallar-variables clausula2))
                  (constantes (mapcar (lambda (x) (gensym 'k)) variables))
                  (unif-k (map-2-ar list variables constantes))
                  (clausula-k (aplicar unif-k clausula2))
            )
   (and
     (<> (setq unif (subsume-literales (first clausula1) (first clausula-k)))
         'fail
     )
     (<> (subsume-literales (aplicar unif (second clausula1))
                            (second clausula-k)
         )
         'fail
     )
   )
)

(de subsumida (clausula clausulas)
   (and clausulas
        (or (subsume (first clausulas) clausula)
            (subsumida clausula (rest clausulas))
        )
   )
)

(de auto-depurar-subsumidas (clausulas)
   (if flag-control-subsuncion
       (rep (length clausulas)
            (if (subsumida (first clausulas)
                           (rest clausulas)
                )
                (setq clausulas (rest clausulas))
                (setq clausulas (rotar clausulas))
            )
       )
   )
   clausulas
)

(de eliminar-subsumidas (clausulas1 clausulas2)
   (if flag-control-subsuncion
       (rep (length clausulas1)
            (if (subsumida (first clausulas1) clausulas2)
                (setq clausulas1 (rest clausulas1))
                (setq clausulas1 (rotar clausulas1))
            )
       )
   )
   clausulas1
)

(print 'subsume-ok)
