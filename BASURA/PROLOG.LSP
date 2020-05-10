
(setq CLAUSULAS '(
      ((append nil ?x1 ?x2))
      ((append (cons ?w2 ?x2) ?y2 (cons ?w2 ?z2))
               (- append ?x2 ?y2 ?z2))
      )
)

; append([],X1,X1).
; append([W2|X2],Y2,[W2|Z2]) :- append(X2,Y2,Z2).

(de prolog (&aux goal variables)
   (forever
     (catch end
        (prin0 '?-)
        (probar (setq goal (read))
                (setq variables (hallar-vars goal))
        )
     )
   )
)

(de probar (&opt (goal goal) (respuesta (hallar-vars goal)) (claus clausulas))
   (if claus
      (let ( (resolvente (resolver goal (first claus))) )
	    (if resolvente
	       (let* ( (nuevo-goal (resolvente 'clausula))
		       (r (renom nuevo-goal))
		       (sust (resolvente 'unificador))
		     )
		     (if nuevo-goal
			(probar (aplicar r nuevo-goal)
				(aplicar r (aplicar sust respuesta))
			)
			(mostrar variables (aplicar sust respuesta))
			(prin0 'mas?)
			(if (<> 's (read)) (throw end))
			(probar goal respuesta (rest claus))
		     )
	       )
	       (probar goal respuesta (rest claus))
	    )
      )
      (print3 "no hay (mas) respuestas")
   )
)
