
(de resolver (clause1 clause2 &opt (resto1 nil) (resto2 nil) &aux unif)
   (if clause1
      (if clause2
	  (if (<> 'fail (setq unif (unificar (first clause1)
					     (negar (first clause2))
	      ) 	)	   )
	      (aplicar unif (append (append resto1 (rest clause1))
				    (append resto2 (rest clause2))
	      ) 	    )
	      (resolver clause1 (rest clause2)
			resto1	(snoc (first clause2) resto2)
	      )
	  )
	  (resolver (rest clause1) resto2
		    (snoc (first clause1) resto1) nil
	  )
      )
      'fail
   )
)

(de negar (expr)
   (if (or (atom expr) (<> (first expr) 'no))
	(list 'no expr)
	(second expr)
   )
)

(resolver   '( (no p)	    (q ?x)   )	
	     '(     p	(no (q	a))  )
)
