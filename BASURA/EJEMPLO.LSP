
(setq er1 '(resolver-completo (@ (q a) <- (p ?y) )
                              (@ (p b) <- (q ?x) )
           )
      er2 '(resolver-completo (@ (q a) (p ?b) <- (r a) (s ?b) )
                              (@ (r ?c) (s d) <- (t ?c) (u d) )
           )
)

(setq es1 '(unificar=> '(p a (q ?x) ?y) '(p a (q b) (r c)))

      es2 '(unificar=> '(p a b c) '(p a b ?x))

      es3 '(subsume-literal '(p ?x a) '((q ?y ?z) (p a ?x) (p ?x a b)))

      es4 '(subsume-literal '(p ?x a) '((q ?x a) (p a a) (p ?y a)))

      es5 '(subsume-literales '((p ?x ?y) (r ?v a))
                              '((q ?x a) (p ?w b) (r ?z a)) )

      es6 '(subsume-literales '((q b a))
                              '((q ?x a) (q a a) (p ?y a)) )

      es7 '(subsume (@ (p ?x a) <- (q b ?y))                      ; si
                    (@ (q ?w a) (p b a) <- (q b ?z) (r a)) )

      es8 '(subsume (@ (p ?x a) <- (q c))                         ; no
                    (@ (q c) (p a a) <- (q ?y)) )

      es9 '(subsumida (@ (p ?x a) (q c a) <- (q u u))             ; si
                      ($ ((r ?x) <- (p ?y))
                         ((q ?v a) (p ?w a) <- (q ?y ?z))
                      ) )

      es10 '(subsumida (@ (p ?x a) <- (q ?y ?x))                  ; no
                       ($ ((q ?z a) (p a b) (p a a) <- (q a b))
                          ((r a) <- (q a b))
                       ) )
)

(setq ef1 '(fusionar (@ (p a) (p ?x)))

      ef2 '(fusionar (@ (p ?x b) (p a ?y)))

      ef3 '(fusionar (@ (p ?x) (p a) <- (q ?y) (q b)))

      ef4 '(fusionar (@ (p ?x) <- (q ?x a) (q b ?y)))

      ef5 '(fusionar (@ (p ?x) (p ?y)))
)
