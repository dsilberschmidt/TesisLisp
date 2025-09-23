
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
       (insert (string #\? x))
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

(de largo (lista)
   (if (listp lista)
       (length lista)
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

(de vtol (expr)
   (cond
       ((vectorp expr)
        (loop for i from 0 below (length expr)
              collect (vtol (aref expr i))))
       ((atom expr) expr)
       (t (cons (vtol (first expr)) (vtol (rest expr))))
   )
)

(de ltov (expr)
   expr
)

(de ltov1 (expr)
   expr
)

(print 'uticla-ok)
