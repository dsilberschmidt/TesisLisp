
(de compactar (lista)
   (if lista
      (append (if (not (pertenece (first lista) (rest lista)))
                  (list (first lista))
              )
              (compactar (rest lista))
      )
   )
)
