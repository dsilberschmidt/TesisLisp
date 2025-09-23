
 ; fusion contiene:   fusiona-literal, fusionar, fusionar-clausulas

 ; (load "unificar")  necesita: unificar, aplicar

 ; (load "uti")       necesita: rotar, reemplazar

(de fusiona-literal (literal literales &aux unif)
   (if literales
      (if (<> (setq unif (unificar literal (first literales))) 'fail)
          unif
          (fusiona-literal literal (rest literales))
      )
      'fail
   )
)

(de fusionar (clausula &aux unif (positivos (first clausula))
                                 (negativos (second clausula))
                                 (respuestas (third clausula))
                                 (hubo-fusion nil)
           )
   (rep (length positivos)
        (setq unif (fusiona-literal (first positivos) (rest positivos)))
        (if (<> unif 'fail)
            (setq positivos (aplicar unif (rest positivos))
                  negativos (aplicar unif negativos)
                  respuestas (aplicar unif respuestas)
                  hubo-fusion t
            )
            (setq positivos (rotar positivos))
        )
   )
   (rep (length negativos)
        (setq unif (fusiona-literal (first negativos) (rest negativos)))
        (if (<> unif 'fail)
            (setq positivos (aplicar unif positivos)
                  negativos (aplicar unif (rest negativos))
                  respuestas (aplicar unif respuestas)
                  hubo-fusion t
            )
            (setq negativos (rotar negativos))
        )
   )
   (concat+ positivos negativos respuestas
            (reemplazar (rest clausula 3) (- 7 3)
                        (if hubo-fusion
                            'fusion
                            (nth clausula 7)
                        )
            )
   )
)

(de fusionar-clausulas (clausulas)
   (if flag-control-fusion
      (mapcar fusionar clausulas)
      clausulas
   )
)

(print 'fusion-ok)
