
 ; clausal contiene:   clausal, clausal1, implout, negin, negar, cuantifout,
 ;                     distribuir, conjout, disyout

 ; (load "uti")        necesita: map-2-ar, union

 ; (load "tautologia") necesita: eliminar-tautologias

 ; (load "subsume")    necesita: auto-depurar-subsumidas

(de clausal (formulas)
   (auto-depurar-subsumidas
      (eliminar-tautologias
         (clausal1 formulas)
      )
   )
)

(de clausal1 (formulas)
  (if formulas
      (append (conjout
                 (distribuir
                    (cuantifout
                       (negin
                          (implout (first formulas))
                       )
                    )
                 )
              )
              (clausal1 (rest formulas))
      )
  )
)

(de implout (formula &aux p q)
   (cond
      (  (atom formula) formula)
      (  (and (= (largo formula) 3)
              (pertenece (second formula) '(xor -> <->))
         )
         (setq p (implout (first formula)))
         (setq q (implout (third formula)))
         (selectq (second formula)
              ( '-> `((not ,p) or ,q))
              ('<-> `(((not ,p) or ,q) and (,p or (not ,q))))
              ('xor `((,p or ,q) and ((not ,p) or (not ,q))))
         )
      )
      (t (cons (implout (first formula)) (implout (rest formula))))
   )
)

(de negin (formula)
   (cond
      (  (atom formula) formula)
      (  (negacion formula) (negar (second formula)))
      (t (cons (negin (first formula)) (negin (rest formula))))
   )
)

(de negar (formula &aux p q)
   (cond
      (  (negacion formula) (negin (second formula)))
      (  (or (conjuncion formula) (disyuncion formula))
         (setq p (negar (first formula)))
         (setq q (negar (third formula)))
         (selectq (second formula)
              ('and `(,p or  ,q))
              ('or  `(,p and ,q))
         )
      )
      (  (or (universal formula) (existencial formula))
         (setq p (negar (third formula)))
         (selectq (first formula)
              ('existe   `(paratodo ,(second formula) ,p))
              ('paratodo `(existe   ,(second formula) ,p))
         )
      )
      (t `(not ,formula))
   )
)

(de cuantifout (formula &opt (param nil) &aux var sust)
   (cond
      (  (atom formula) formula)
      (  (universal formula)
         (setq var (variabilizar (second formula)))
         (setq sust (map-2-ar list (second formula) var))
         (sustituir sust (cuantifout (third formula) (append param var)))
      )
      (  (existencial formula)
         (setq sust (skolemizar (second formula) param))
         (sustituir sust (cuantifout (third formula) param))
      )
      (t (cons (cuantifout (first formula) param)
               (cuantifout (rest formula) param)
         )
      )
   )
)

(de distribuir (formula &aux p q u v)
   (cond
      (  (atom formula) formula)
      (  (disyuncion formula)
         (setq p (distribuir (first formula)))
         (setq q (distribuir (third formula)))
         (if (conjuncion p) (setq v p p q q v))
         (if (not (conjuncion q))
             `(,p or ,q)
             (setq u (distribuir `(,p or ,(first q))))
             (setq v (distribuir `(,p or ,(third q))))
             `(,u and ,v)
         )
      )
      (t (cons (distribuir (first formula)) (distribuir (rest formula))))
   )
)

(de conjout (formula)
   (if (conjuncion formula)
       (append (conjout (first formula)) (conjout (third formula)))
       (list (disyout formula))
   )
)

(de disyout (formula &aux p q)
   (if (not (disyuncion formula))
       (if (negacion formula)
           (list nil (rest formula))
           (list (list formula) nil)
       )
       (setq p (disyout (first formula)))
       (setq q (disyout (third formula)))
       (map-2-ar union p q)
   )
)

(print 'clausal-ok)
