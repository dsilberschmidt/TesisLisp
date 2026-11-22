
 ; unificar contiene: aplicar, valor, variablep, componer, aplicar-term,
 ;                    pares-nuevos, ocurre, unificar

(de aplicar (sust expr)
   (cond
      ((variablep expr) (valor sust expr))
      ((atom expr) expr)
      (t (cons (aplicar sust (first expr))
               (aplicar sust (rest expr))
         )
      )
   )
)

(de valor (sust var)
   (cond
      ((null sust) var)
      ((= (first (first sust)) var) (second (first sust)))
      (t (valor (rest sust) var))
   )
)

(de variablep (expr)
   (and (symbolp expr)
        (= (nth (pname expr) 1) \?)
   )
)

(de componer (sust1 sust2)
   (append (aplicar-term sust1 sust2)
           (pares-nuevos sust1 (mapcar first sust2))
   )
)

(de aplicar-term (sust1 sust2)
   (if sust2
       (cons (list (first (first sust2))
                   (aplicar sust1 (second (first sust2)))
             )
             (aplicar-term sust1 (rest sust2))
       )
   )
)

(de pares-nuevos (sust lista)
  (cond
    ((null sust) nil)
    ((pertenece (first (first sust)) lista) (pares-nuevos (rest sust) lista))
    (t (cons (first sust) (pares-nuevos (rest sust) lista)))
  )
)

(de ocurre (var expr)
   (if (atom expr)
       (= var expr)
       (or (ocurre var (first expr))
           (ocurre var (rest  expr))
       )
   )
)

(de unificar (literal1 literal2 &aux unif1 unif2)
  (cond
     ( (= literal1 literal2) nil)
     ( (and (variablep literal1) (not (ocurre literal1 literal2)))
                                 (list (list literal1 literal2))
     )
     ( (and (variablep literal2) (not (ocurre literal2 literal1)))
                                 (list (list literal2 literal1))
     )
     ( (and (listp literal1) (listp literal2)
            (<> (setq unif1 (unificar (first literal1) (first literal2)))
                'fail
            )
            (<> 'fail (setq unif2 (unificar (aplicar unif1 (rest literal1))
                                            (aplicar unif1 (rest literal2))
                                  )
                      )
            )
       )
       (componer unif2 unif1)
     )
     (t 'fail)
  )
)

(print 'unificar-ok)