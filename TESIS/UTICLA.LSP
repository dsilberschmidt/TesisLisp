
; uticla contiene: skolemizar, variabilizar, chequear-variable,
;                  sustituir, conjuncion, disyuncion, negacion,
;                  universal, existencial, largo, vtol, ltov, ltov1

; (load "unificar")   necesita: valor, variablep

; (load "renombrar")  necesita: gensym

(de skolemizar (existenciales universales)
   (mapcar (lambda (x) (list x (cons (gensym 'sk) universales)))
           existenciales
   )
)

(de variabilizar (universales)
   (mapcar (lambda (x) (gensym (chequear-variable x)))
           universales
   )
)

(de chequear-variable (x)
   (if (variablep x)
       x
       (insert (string \? x))
   )
)

(de sustituir (sust expr)
   (cond
      ((atom expr) (valor sust expr))
      (t (cons (sustituir sust (first expr))
               (sustituir sust (rest expr))
         )
      )
   )
)

(de conjuncion (formula)
   (and (= (largo formula) 3) (eq (second formula) 'and))
)

(de disyuncion (formula)
   (and (= (largo formula) 3) (eq (second formula) 'or))
)

(de negacion (formula)
   (and (= (largo formula) 2) (eq (first formula) 'not))
)

(de universal (formula)
   (and (= (largo formula) 3) (eq (first formula) 'paratodo))
)

(de existencial (formula)
   (and (= (largo formula) 3) (eq (first formula) 'existe))
)

(de largo (lista)
   (if (listp lista)
       (length lista)
   )
)

(de vtol (expr)
   (cond
       ((vectorp expr)
        (do ((posicion (length expr) (sub1 posicion))
             (lista nil (list 'cons (vtol (expr posicion)) lista)))
            ((< posicion 1) lista)
        )
       )
       ((atom expr) expr)
       (t (cons (vtol (first expr)) (vtol (rest expr))))
   )
)

(de ltov (expr)
   (cond
      ((atom expr) expr)
      ((and (= (length expr) 3)
            (eq (first expr) 'cons)
       )
       (ltov1 expr)
      )
      (t (cons (ltov (first expr)) (ltov (rest expr))))
   )
)

(de ltov1 (expr)
   (cond
      ((null expr) #())
      ((atom expr) (vector '| expr))
      (t (vector (newvector 1 (ltov (second expr))) (ltov1 (third expr))))
   )
)

(print 'uticla-ok)
