
;;; LGC Lenguaje de Programacion Logica

;;; (load "uti")
;;; (load "unificar")
;;; (load "renombrar")
;;; (load "resolver")
;;; (load "tautologia")
;;; (load "fusion")
;;; (load "subsume")
;;; (load "monitor")
;;; (load "estrategia")
;;; (load "respues")
;;; (load "busqueda")
;;; (load "inicial")
;;; (load "interprete")
;;; (load "uticla")
;;; (load "clausal")
;;; (load "reciclar")
;;; (load "evaluar")
(terpri)
;(print 'LGC-ok)


 ;;; uti contiene: cabeza, last_n, posicion, rotar, snoc, reemplazar,
 ;               separar, implicar, armar-clausula, numerar,
 ;               map-2-ar, union_n, interseccion, compactar,
 ;               compactar-orden, lisp, quit_n

(defun cabeza (lista numero)
   (if (and (> numero 0) lista)
       (cons (first lista) (cabeza (rest lista) (sub1 numero)))
   )
)
(defun sub1 (numero)
  (- numero 1)
)

(defun add1 (numero)
  (+ numero 1)
)

(defun last_n (lista)
   (first (reverse lista))
)

(defun pertenece (elemento lista)
    (member elemento lista)
)

(defun posicion (elemento lista &optional (lugar 1))
   (if lista
      (if (= (first lista) elemento)
          lugar
          (posicion elemento (rest lista) (add1 lugar))
      )
   )
)

(defun rotar (lista)
   (snoc (first lista) (rest lista))
)

(defun snoc (expr lista)
   (append lista (list expr))
)

(defun reemplazar (lista numero expresion)
   (append+ (cabeza lista (sub1 numero))
            (list expresion)
            (rest lista numero)
   )
)

(defun separar (implicacion &optional (positivos nil))
   (if (and (<> (first implicacion) '<-) implicacion)
       (separar (rest implicacion) (snoc (first implicacion) positivos))
       (list positivos (rest implicacion))
   )
)

(defun implicar (clausulas)
   (mapcar (lambda (x) (append (first x) (cons '<- (second x))))
           clausulas
   )
)

(defun armar-clausula (clausula &optional (padre1 nil) (padre2 nil)
                                  (profundidad 0)
                             &aux (positivos (first clausula))
                                  (negativos (second clausula))
                                  (respuestas (third clausula))
                                  (tipo 'clausula)
                                  (numero (numerar))
                   )
   (list positivos negativos respuestas
         padre1 padre2 profundidad tipo numero
   )
)

(defun numerar ()
   (setq nro-clausulas (add1 nro-clausulas))
)

(defun map-2-ar (funcion lista1 lista2)
   (if (and lista1 lista2)
       (cons (funcion (first lista1) (first lista2))
             (map-2-ar funcion (rest lista1) (rest lista2))
       )
   )
)

(defun mappend (funcion lista)
   (if lista
       (append (funcion (first lista))
               (mappend funcion (rest lista))
       )
   )
)

(defun union_n (lista1 lista2)
   (compactar-orden (append lista1 lista2))
)

(defun interseccion (conjunto1 conjunto2)
   (if conjunto1
      (if (pertenece (first conjunto1) conjunto2)
          (cons (first conjunto1) (interseccion (rest conjunto1) conjunto2))
          (interseccion (rest conjunto1) conjunto2)
      )
   )
)

(defun compactar (lista)
   (if lista
       (if (pertenece (first lista) (rest lista))
           (compactar (rest lista))
           (cons (first lista) (compactar (rest lista)))
       )
   )
)

(defun compactar-orden (lista)
   (reverse (compactar (reverse lista)))
)

(defun lisp ()
   (prin2 '>>>)
   (if (<> (print (eval (read))) 'quit)
       (lisp)
   )
)

(defun quit_n () 'quit)

(print 'uti-ok)

 ; unificar contiene: aplicar, valor, variablep, componer, aplicar-term,
 ;                    pares-nuevos, ocurre, unificar

(defun aplicar (sust expr)
   (cond
      ((variablep expr) (valor sust expr))
      ((atom expr) expr)
      (t (cons (aplicar sust (first expr))
               (aplicar sust (rest expr))
         )
      )
   )
)

(defun valor (sust var)
   (cond
      ((null sust) var)
      ((= (first (first sust)) var) (second (first sust)))
      (t (valor (rest sust) var))
   )
)

(defun variablep (expr)
   (and (symbolp expr)
        (= (nth (pname expr) 1) \?)
   )
)

(defun componer (sust1 sust2)
   (append (aplicar-term sust1 sust2)
           (pares-nuevos sust1 (mapcar first sust2))
   )
)

(defun aplicar-term (sust1 sust2)
   (if sust2
       (cons (list (first (first sust2))
                   (aplicar sust1 (second (first sust2)))
             )
             (aplicar-term sust1 (rest sust2))
       )
   )
)

(defun pares-nuevos (sust lista)
  (cond
    ((null sust) nil)
    ((pertenece (first (first sust)) lista) (pares-nuevos (rest sust) lista))
    (t (cons (first sust) (pares-nuevos (rest sust) lista)))
  )
)

(defun ocurre (var expr)
   (if (atom expr)
       (= var expr)
       (or (ocurre var (first expr))
           (ocurre var (rest  expr))
       )
   )
)

(defun unificar (literal1 literal2 &aux unif1 unif2)
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

(print 'unificar-ok)
 ; renombrar contiene:  renombrar, gensym_n, extraer-raiz, hallar-variables,
 ;                      hallar-raices, normalizar, inicializar-gensym_n,
 ;                      renombrar-anonimas, sacar-anonimas

 ; (load "unificar")   necesita: variablep, aplicar

 ; (load "uti")        necesita: union_n, map-2-ar

 ; (load "uticla")     necesita: renombrar-anonima

(defun renombrar (expresion &aux (variables (hallar-variables expresion)))
   (aplicar (map-2-ar list variables (mapcar gensym_n variables)) expresion)
)

(defun gensym_n (simbolo &aux (raiz (extraer-raiz simbolo)) posicion)
   (if (setq posicion (memvec raiz vector-raices))
       (insert (string raiz \_ ( (store vector-numeros
                                        posicion
                                        (add1 (vector-numeros posicion))
                                 )
                                 posicion
                               )
               )
        )
        (setq vector-raices (vector vector-raices simbolo))
        (setq vector-numeros (vector vector-numeros 1))
        (insert (string simbolo \_ 1))
    )
)

(defun extraer-raiz (simbolo &aux (hilera (pname simbolo))
                               (posicion (string-search hilera \_))
                 )
   (insert (if posicion
               (substring hilera 1 (sub1 posicion))
               hilera
           )
   )
)

(defun hallar-variables (expresion)
   (if (variablep expresion)
       (list expresion)
       (if (listp expresion)
           (union_n (hallar-variables (first expresion))
                  (hallar-variables (rest expresion))
           )
       )
   )
)

(defun hallar-raices (expresion)
   (mapcar extraer-raiz (hallar-variables expresion))
)

(defun normalizar (clausulas)
   (mapcar renombrar (renombrar-anonima clausulas))
)

(defun inicializar-gensym_n ()
   (defvar vector-raices nil)     ;originalmente [] en lugar de nil
   (defvar vector-numeros nil)    ;originalmente [] en lugar de nil
)

(defun renombrar-anonima (expr)
   (cond
      ((= expr '?) (gensym_n '?))
      ((atom expr) expr)
      (t (cons (renombrar-anonima (first expr))
               (renombrar-anonima (rest expr))
         )
      )
   )
)

(defun sacar-anonimas (lista)
   (cond
      ( (null lista) nil )
      ( (= (extraer-raiz (first lista)) '?) (sacar-anonimas (rest lista)) )
      ( t  (cons (first lista) (sacar-anonimas (rest lista))) )
   )
)

(print 'renombrar-ok)


 ; resolver contiene:  resolver-diagonal, resolver-completo, nueva-prof

 ; (load "unificar")   necesita: unificar, aplicar

 ; (load "uti")        necesita: snoc, armar-clausula

 ; (load "renombra")   necesita: renombrar

(defun resolver-diagonal (pos1 neg1 pos2 neg2 respuestas
                       &optional (aux-pos1 nil) (aux-neg2 nil) &aux unif
                      )
  (if pos1
    (if neg2
      (if (<> (setq unif (unificar (first pos1) (first neg2))) 'fail)
          (cons (renombrar (aplicar unif
                                    (list (append+ aux-pos1 (rest pos1) pos2)
                                          (append+ neg1 aux-neg2 (rest neg2))
                                          respuestas
                                    )
                       )
                )
                (resolver-diagonal pos1 neg1 pos2 (rest neg2) respuestas
                                   aux-pos1 (snoc (first neg2) aux-neg2)
                )
          )
          (resolver-diagonal pos1 neg1 pos2 (rest neg2) respuestas
                             aux-pos1 (snoc (first neg2) aux-neg2)
          )
      )
      (resolver-diagonal (rest pos1) neg1 pos2 aux-neg2 respuestas
                         (snoc (first pos1) aux-pos1) nil
      )
    )
  )
)

(defun resolver-completo (clausula1 clausula2
                       &aux (profundidad (nueva-prof clausula1 clausula2))
                      )
  (mapcar (lambda (x) (armar-clausula x clausula1 clausula2 profundidad))
          (append (resolver-diagonal
                       (first clausula1) (second clausula1)
                       (first clausula2) (second clausula2)
                       (union_n (third clausula1) (third clausula2))
                  )
                  (resolver-diagonal
                       (first clausula2) (second clausula2)
                       (first clausula1) (second clausula1)
                       (union_n (third clausula1) (third clausula2))
                  )
          )
  )
)

(defun nueva-prof (clausula1 clausula2)
   (add1 (max (sixth clausula1) (sixth clausula2)))
)

(print 'resolver-ok)
 ; tautologia contiene: tautologiap, eliminar-tautologias

 ; (load "uti")         necesita: interseccion


(defun tautologiap (clausula)
   (and flag-control-tautologia
        (interseccion (first clausula) (second clausula))
   )
)

(defun eliminar-tautologias (clausulas)
   (if clausulas
      (if (tautologiap (first clausulas))
          (eliminar-tautologias (rest clausulas))
          (cons (first clausulas) (eliminar-tautologias (rest clausulas)))
      )
   )
)

(print 'tautologia-ok)

 ; fusion contiene:   fusiona-literal, fusionar, fusionar-clausulas

 ; (load "unificar")  necesita: unificar, aplicar

 ; (load "uti")       necesita: rotar, reemplazar

(defun fusiona-literal (literal literales &aux unif)
   (if literales
      (if (<> (setq unif (unificar literal (first literales))) 'fail)
          unif
          (fusiona-literal literal (rest literales))
      )
      'fail
   )
)

(defun fusionar (clausula &aux unif (positivos (first clausula))
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

(defun fusionar-clausulas (clausulas)
   (if flag-control-fusion
      (mapcar fusionar clausulas)
      clausulas
   )
)

(print 'fusion-ok)

 ; subsume contiene:   subsume-literal, subsume-literales, unificar=>
 ;                     subsume, subsumida, eliminar-subsumidas
 ;                     auto-depurar-subsumidas

 ; (load "unificar")   necesita: variablep, ocurre, aplicar, componer

 ; (load "uti")        necesita: rotar

(defun unificar=> (literal1 literal2
                &aux unif (variables (hallar-variables literal2))
                     (constantes (mapcar (lambda (x) (gensym_n 'k)) variables))
                     (unif1 (map-2-ar list variables constantes))
                     (unif2 (map-2-ar list constantes variables))
               )
   (if (<> (setq unif (unificar literal1 (aplicar unif1 literal2))) 'fail)
       (sustituir unif2 unif)
       'fail
   )
)

(defun subsume-literal (literal literales &aux unif)
   (if literales
      (if (<> (setq unif (unificar literal (first literales))) 'fail)
          unif
          (subsume-literal literal (rest literales))
      )
      'fail
   )
)

(defun subsume-literales (lit1 lit2 &aux unif1 unif2)       ; lit = literales
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

(defun subsume (clausula1 clausula2
             &aux unif (variables (hallar-variables clausula2))
                  (constantes (mapcar (lambda (x) (gensym_n 'k)) variables))
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

(defun subsumida (clausula clausulas)
   (and clausulas
        (or (subsume (first clausulas) clausula)
            (subsumida clausula (rest clausulas))
        )
   )
)

(defun auto-depurar-subsumidas (clausulas)
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

(defun eliminar-subsumidas (clausulas1 clausulas2)
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

 ; monitor contiene: monitor, prin-elementos, pp-clausula, pp-clausulas,
 ;                   recorrer-ancestros, interrupcion, shell

 ; (load "uti")      necesita: posicion

(defun monitor (expandidas expandibles)
   (if flag-monitor
       (progn (print "expandidas")
              (pp-clausulas expandidas)
              (terpri)
              (print "expandibles")
              (pp-clausulas expandibles)
              (terpri)
       )
   )
)

(defun prin-elementos (lista)
   (if lista
       (progn (prin2 (first lista))
              (prin-elementos (rest lista))
       )
   )
)

(defun pp-clausula (clausula &optional (columna 0) &rest opcion)
   (prin2 (newstring columna \ ))
   (prin-elementos (list (sixth clausula) \:))
   (prin-elementos (first clausula))
   (prin2  '<- )
   (prin-elementos (second clausula))
   (if (pertenece 'v opcion)
       (progn (prin2 "       [")
              (prin-elementos (third clausula))
              (prin2 \])
       )
   )
   (terpri)
)

(defun pp-clausulas (clausulas)
   (mapc (lambda (x) (pp-clausula x 9 'v)) clausulas)
   clausulas
)

(defun recorrer-ancestros (clausula)
   (if clausula
       (progn
           (terpri) (pp-clausula (fourth clausula) 0 'v)
           (terpri) (pp-clausula (fifth clausula) 0 'v)
           (terpri) (pp-clausula clausula 9 'v)
           (setq tecla
                 (do ((char (prin2 "Opcion (1, 2, V, Q):") (console-in)))
                     (((pertenece char '(\1 \2 \v \q)) (print char)))
                 )
           )
           (if (selectq tecla
                    (\1      (recorrer-ancestros (fourth clausula)))
                    (\2      (recorrer-ancestros (fifth clausula)))
                    ((\v \q) nil)
               )
               (recorrer-ancestros clausula)
               (= tecla \v)
           )
       )
       (print "Clausula base.")
   )
)

(defun interrupcion ()
   (if flag-interrupcion
       (do ((char (prin2 "Interrupcion:") (console-in)))
           (((= char \ ) (print2 "cont")))
           (if (<> (shell char) 'nada)
               (prin2 "Interrupcion:")
           )
       )
   )
)

(defun shell (char &optional (clausula nil))
   (cond
      ( (= char \l) (lisp))
      ( (and (= char \r) clausula) (recorrer-ancestros clausula))
      ( t 'nada)
   )
)

(print 'monitor-ok)

 ; estrategia contiene: ancho, soporte, sin-restriccion, entrada-lineal,
 ;                      restriccion-fusion, tope-profundidad, ancestro,
 ;                      constante, menores, unidades, horizontal, vertical

 ; tipos de llamada

(defun ancho (base goal)
   (busqueda nil (append base goal) (sacar-anonimas (hallar-raices goal)))
)

(defun soporte (base goal)
   (busqueda base goal (sacar-anonimas (hallar-raices goal)))
)

 ; tipos de seleccion

(defun sin-restriccion (clausula) t)

(defun entrada-lineal (clausula)
   (= (sixth clausula) 0)
)

(defun restriccion-fusion (clausula)
   (or (= (seventh clausula) 'fusion)
       (= (sixth clausula) 0)
   )
)

 ; control de profundidad

(defun tope-profundidad (clausula)
   (if limite-profundidad
       (= limite-profundidad (sixth clausula))
   )
)

 ; filtrado de ancestros

(defun ancestro (clausula1 clausula2)
   (and clausula2
        (or clausula1 clausula2)
            (ancestro clausula1 (fourth clausula2))
            (ancestro clausula1 (fifth clausula2))
        )
   )


 ; tipos de funcion-h

(defun constante (clausula) 0)

(defun menores (clausula)
   (+ (length (first clausula)) (length (second clausula)))
)

(defun unidades (clausula)
   (if (= (+ (length (first clausula)) (length (second clausula))) 1)
       0
       1
   )
)
 ; tipos de prioridad

(defvar horizontal '<)     ;originalmente setq y <

(defvar vertical '<=)      ;originalmente setq y <=

(print 'estrategia-ok)
 ; respuesta contiene: eliminar-respuestas-y-mostrarlas, clausula-no-nula,
 ;                     interactuar, mostrar-solucion, pp-solucion

(defun eliminar-respuestas-y-mostrarlas
         (resolventes raices &aux (primer-resolvente (first resolventes))
                                  (solucion (third primer-resolvente))
         )                         ; solucion es una posible solucion
  (if resolventes
     (if (clausula-no-nula primer-resolvente)
        (cons primer-resolvente
              (eliminar-respuestas-y-mostrarlas (rest resolventes) raices)
        )
        (if (and (> (length solucion) 1)
                 flag-reciclaje
                 raices                      ; sacable por union_n
                 (reciclar raices solucion)
            )
            (if flag-mas
                (eliminar-respuestas-y-mostrarlas (rest resolventes) raices)
            )
            (if (= (length solucion) 1)
                (setq supuestos-exitosos
                      (cons (first solucion) supuestos-exitosos)
                )
            )
            (mostrar-solucion raices solucion)
            (if (= 'si (interactuar primer-resolvente))
                (eliminar-respuestas-y-mostrarlas (rest resolventes) raices)
                (setq flag-mas nil)
            )
        )
     )
  )
)

(defun interactuar (&optional (clausula nil))
   (do ((char (prin2 "Mas (s/n)?") (console-in)))
       (((= char \s) (print 'si)) ((= char \n) (print 'no)))
       (if (<> (shell char clausula) 'nada)
           (prin2 "Mas (s/n)?")
       )
   )
)

(defun mostrar-solucion (variables solucion)
   (if (null variables)
       (print2 "Si.")
       (pp-solucion variables (first solucion))
       (if (rest solucion)
           (progn (print2 "O bien.")
                  (mostrar-solucion variables (rest solucion))
           )
       )
   )
)

(defun pp-solucion (variables solucion)
   (map-2-ar (lambda (x y) (prin2 x) (prin2 '=) (print y))
             variables
             (ltov solucion)
   )
)

(defun clausula-no-nula (clausula)
   (or (first clausula) (second clausula))
)

(print 'respuesta-ok)
 ; busqueda contiene: busqueda, obtener-expandidas, obtener-expandibles,
 ;                    colocar, ordenar, expandir, generar-resolventes,
 ;                    admisibles

(defun  busqueda (expandidas expandibles raices &aux resolventes (flag-mas t))
   (monitor expandidas expandibles)
   (interrupcion)
   (if (null expandibles)
       (print2 "No hay (mas) respuestas.")
       (setq resolventes
             (if (not (tope-profundidad (first expandibles)))
                 (generar-resolventes expandidas expandibles raices)
             )
       )
       (if flag-mas
           (busqueda (obtener-expandidas expandidas
                                         (first expandibles)
                                         heuristica-secundaria
                     )
                     (obtener-expandibles expandibles
                                          (admisibles resolventes
                                                      heuristica-primaria
                                          )
                                          heuristica-primaria
                     )
                     raices
           )
       )
   )
)

(defun obtener-expandidas (expandidas-viejas aspirante heuristica
                        &aux (admisibilidad (first heuristica))
                       )
   (if (and (admisibilidad aspirante)
            (not (subsumida aspirante expandidas-viejas))
       )
       (colocar aspirante
                (eliminar-subsumidas expandidas-viejas (list aspirante))
                heuristica
       )
       expandidas-viejas
   )
)

(defun obtener-expandibles (expandibles-viejas resolventes heuristica)
   (ordenar resolventes
            (eliminar-subsumidas (rest expandibles-viejas) resolventes)
            heuristica
   )
)

(defun colocar (elemento lista heuristica &aux (funcion-h (second heuristica))
                                            (prioridad (third heuristica))
            )
   (if (or (null lista)
           (prioridad (funcion-h elemento) (funcion-h (first lista)))
       )
       (cons elemento lista)
       (cons (first lista)
             (colocar elemento (rest lista) heuristica)
       )
   )
)

(defun ordenar (lista lista-ordenada heuristica)
   (if lista
       (ordenar (rest lista)
                (colocar (first lista) lista-ordenada heuristica)
                heuristica
       )
       lista-ordenada
   )
)

(defun expandir (clausula clausulas)
   (if clausulas
       (if (or (not flag-filtrado-ancestros)
               (= (sixth (first clausulas)) 0)
               (ancestro clausula (first clausulas))
           )
           (append (resolver-completo clausula (first clausulas))
                   (expandir clausula (rest clausulas))
           )
           (expandir clausula (rest clausulas))
       )
   )
)

(defun generar-resolventes (expandidas expandibles raices)
   (auto-depurar-subsumidas
      (eliminar-subsumidas
         (eliminar-respuestas-y-mostrarlas               ; modifica flag-mas
            (fusionar-clausulas
               (eliminar-tautologias
                   (expandir (first expandibles) expandidas)
               )
            )
            raices
         )
         (append expandidas (rest expandibles))
      )
   )
)

(defun admisibles (lista heuristica &aux (admisibilidad (first heuristica)))
   (if lista
       (if (admisibilidad (first lista))
           (cons (first lista) (admisibles (rest lista) heuristica))
           (admisibles (rest lista) heuristica)
       )
   )
)

(print 'busqueda-ok)

 ; inicial contiene: inicializacion, seth, on, on

(defun inicializacion ()
   (inicializar-gensym_n)
   (defvar nro-clausulas 0)

   (defvar  flag-control-tautologia t)
   (defvar  flag-control-fusion t)
   (defvar  flag-control-subsuncion t)
   (defvar  flag-monitor t)
   (defvar  flag-interrupcion t)
   (defvar  flag-reciclaje t)

   (defvar llamada 'soporte)                 ; (eval ([soporte ancho] 1)))

   (defvar admisibilidad-1 'sin-restriccion) ; (eval ([sin-restriccion] 1)))
   (defvar funcion-h-1 'unidades)            ; (eval ([constante menores unidades] 3)))
   (defvar prioridad-1 'horizontal)          ; (eval ([vertical horizontal] 2)))

   (defvar admisibilidad-2 'sin-restriccion) ; (eval ([sin-restriccion entrada-lineal restriccion-fusion] 1))

   (defvar funcion-h-2 'constante)           ; (eval ([constante menores unidades] 1)))
   (defvar prioridad-2 'horizontal)          ; (eval ([vertical horizontal] 2)))

;   (seth)
   (defvar heuristica-primaria
         (list admisibilidad-1 funcion-h-1 prioridad-1)
   )
   (defvar heuristica-secundaria
         (list admisibilidad-2 funcion-h-2 prioridad-2)
   )
   (defvar limite-profundidad nil)

   (defvar flag-filtrado-ancestros nil)
)

;;;(defun seth ()
;;;   (defvar heuristica-primaria
;;;         (list admisibilidad-1 funcion-h-1 prioridad-1)
;;;   )
;;;   (defvar heuristica-secundaria
;;;         (list admisibilidad-2 funcion-h-2 prioridad-2)
;;;   )
;;;)

(defun on (flags)                       ;originalmente df
   (mapc (lambda (flag) (set flag t))
         flags
   )
)

(defun off (flags)                      ;originalmente df
   (mapc (lambda (flag) (set flag nil))
         flags
   )
)

(inicializacion)

(print 'inicial-ok)
 ; interprete contiene: interprete, compilar, armar-clausulas, armar-goal
 ;                      add, i, c

(defun interprete (&optional (base-objeto objeto)
                     (goal-fuente (progn (prin2 '?-) (read)))
                &aux (base-interna (armar-clausulas base-objeto))
                     (goal-interno (armar-goal goal-fuente))
                     (supuestos-exitosos nil)
                     (supuestos-fracasados nil)
               )
   (selectq goal-fuente
       ( 'exit   nil)
       ( 'lisp   (print2 "Use (quit_n) para retornar al sistema.")
                 (lisp)  (interprete base-objeto)
       )
       ( 'add    (interprete (add base-objeto (read))))
       ( 'list   (pp-clausulas base-interna)
                 (interprete base-objeto)
       )
       ( 'on     (eval `(on  ,(read))) (interprete base-objeto))
       ( 'off    (eval `(off ,(read))) (interprete base-objeto))
       ( t       (funcall llamada base-interna goal-interno)
                 (interprete base-objeto)
       )
   )
)

(defun compilar (base-fuente &optional (base-objeto 'objeto))
   (set base-objeto
        (implicar (clausal base-fuente))
   )
)


(defun armar-clausulas (implicaciones)
   (ordenar
        (normalizar (mapcar (lambda (implicacion)
                                    (armar-clausula (separar implicacion))
                            )
                            (vtol implicaciones)
                    )
        )
        nil
        heuristica-secundaria
   )
)

(defun armar-goal (goal-fuente
                 &aux (goal (clausal (list (negar goal-fuente))))
                      (variables (sacar-anonimas (hallar-variables goal)))
               )
   (ordenar
        (normalizar (mapcar armar-clausula
                            (mapcar (lambda (clausula)
                                            (snoc (list variables) clausula)
                                    )
                                    (vtol goal)
                            )
                    )
        )
        nil
        heuristica-primaria
   )
)

(defun add (base formula)
   (append base (implicar (clausal (list formula))))
)

(defvar i 'interprete)

(defvar c 'compilar)

(print 'interprete-ok)

 ; uticla contiene: skolemizar, variabilizar, chequear-variable,
 ;                  sustituir, conjuncion, disyuncion, negacion,
 ;                  universal, existencial, largo, vtol, ltov, ltov1

 ; (load "unificar")   necesita: valor, variablep

 ; (load "renombrar")  necesita: gensym_n

(defun skolemizar (existenciales universales)
   (mapcar (lambda (x) (list x (cons (gensym_n 'sk) universales)))
           existenciales
   )
)

(defun variabilizar (universales)
   (mapcar (lambda (x) (gensym_n (chequear-variable x)))
           universales
   )
)

(defun chequear-variable (x)
   (if (variablep x)
       x
       (insert (string \? x))
   )
)

(defun sustituir (sust expr)
   (cond
      ((atom expr) (valor sust expr))
      (t (cons (sustituir sust (first expr))
               (sustituir sust (rest expr))
         )
      )
   )
)

(defun conjuncion (formula)
   (and (= (largo formula) 3) (= (second formula) 'and))
)

(defun disyuncion (formula)
   (and (= (largo formula) 3) (= (second formula) 'or))
)

(defun negacion (formula)
   (and (= (largo formula) 2) (= (first formula) 'not))
)

(defun universal (formula)
   (and (= (largo formula) 3) (= (first formula) 'paratodo))
)

(defun existencial (formula)
   (and (= (largo formula) 3) (= (first formula) 'existe))
)

(defun largo (lista)
   (if (listp lista)
       (length lista)
   )
)

(defun vtol (expr)
   (cond
       ( (vectorp expr)
         (do ( (posicion (length expr) (sub1 posicion))
               (lista  nil (list 'cons (vtol (expr posicion)) lista))
             )
             ( ( (< posicion 1) lista) )
         )
       )
       ( (atom expr) expr)
       ( t (cons (vtol (first expr)) (vtol (rest expr))))
   )
)

(defun ltov (expr)
   (cond
      (  (atom expr) expr)
      (  (and (= (length expr) 3)
              (= (first expr) 'cons)
         )
         (ltov1 expr)
      )
      (t (cons (ltov (first expr)) (ltov (rest expr))))
   )
)

(defun ltov1 (expr)
   (cond
      (  (null expr) '[])
      (  (atom expr) (vector '| expr))
      (t (vector (newvector 1 (ltov (second expr))) (ltov1 (third expr))))
   )
)

(print 'uticla-ok)

 ; clausal contiene:   clausal, clausal1, implout, negin, negar, cuantifout,
 ;                     distribuir, conjout, disyout

 ; (load "uti")        necesita: map-2-ar, union_n

 ; (load "tautologia") necesita: eliminar-tautologias

 ; (load "subsume")    necesita: auto-depurar-subsumidas

(defun clausal (formulas)
   (auto-depurar-subsumidas
      (eliminar-tautologias
         (clausal1 formulas)
      )
   )
)

(defun clausal1 (formulas)
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

(defun implout (formula &aux p q)
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

(defun negin (formula)
   (cond
      (  (atom formula) formula)
      (  (negacion formula) (negar (second formula)))
      (t (cons (negin (first formula)) (negin (rest formula))))
   )
)

(defun negar (formula &aux p q)
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

(defun cuantifout (formula &optional (param nil) &aux var sust)
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

(defun distribuir (formula &aux p q u v)
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

(defun conjout (formula)
   (if (conjuncion formula)
       (append (conjout (first formula)) (conjout (third formula)))
       (list (disyout formula))
   )
)

(defun disyout (formula &aux p q)
   (if (not (disyuncion formula))
       (if (negacion formula)
           (list nil (rest formula))
           (list (list formula) nil)
       )
       (setq p (disyout (first formula)))
       (setq q (disyout (third formula)))
       (map-2-ar union_n p q)
   )
)

(print 'clausal-ok)

 ; reciclar  contiene: reciclar, goal-instanciado, incluido

 ; uso de globales: primer-resolvente

(defun reciclar (variables solucion &optional (hubo-reciclaje nil)
                                 &aux (supuesto (first solucion))
             )
   (if solucion
       (if (incluido supuesto supuestos-fracasados)
           (reciclar variables (rest solucion) hubo-reciclaje)
           (if (or (incluido supuesto supuestos-exitosos)
                   (evaluar base-interna (print (goal-instanciado supuesto)))
               )
               (progn (setq supuestos-exitosos
                            (cons supuesto supuestos-exitosos)
                      )
                      (pp-solucion variables supuesto)
                      (if (= (interactuar primer-resolvente) 'si)
                          (reciclar variables (rest solucion) 'true)
                          (setq flag-mas nil)
                          'true
                      )
               )
               (setq supuestos-fracasados
                     (cons supuesto supuestos-fracasados)
               )
               (reciclar variables (rest solucion) hubo-reciclaje)
           )
       )
       hubo-reciclaje
   )
)

;;; (defun crear-base ()                              out
;;;   (append base-interna
;;;           (mappend goal-afirmado supuestos-exitosos)
;;;   )
;;;)

;;;(defun goal-afirmado (supuesto)                    out
;;;)

(defun goal-instanciado (supuesto
                        &aux (variables (first (third (first goal-interno))))
                     )
   (aplicar (map-2-ar list variables supuesto)
            (mapcar (lambda (x)(reemplazar x 3 nil))
                    goal-interno
            )
   )
)

(defun incluido (literal literales)
   (if literales
       (or (<> (unificar=> (first literales) literal) 'fail)
           (incluido literal (rest literales))
       )
   )
)

(print 'reciclar-ok)

 ; evaluar contiene:  evaluar, generar-resolventes-bis, pertenece-vacia,
 ;                    pertenece-vacia-bis

(defun evaluar (expandidas expandibles &aux resolventes)
   (monitor expandidas expandibles)
   (interrupcion)
   (if expandibles
       (if (pertenece-vacia-bis
               (setq resolventes
                     (if (not (tope-profundidad (first expandibles)))
                         (generar-resolventes-bis expandidas expandibles)
                     )
               )
           )
           'true
           (evaluar (obtener-expandidas expandidas
                                        (first expandibles)
                                        heuristica-secundaria
                    )
                    (obtener-expandibles expandibles
                                         (admisibles resolventes
                                                     heuristica-primaria
                                         )
                                         heuristica-primaria
                    )
           )
       )
   )
)

(defun generar-resolventes-bis (expandidas expandibles)
   (auto-depurar-subsumidas
      (eliminar-subsumidas
         (fusionar-clausulas
            (eliminar-tautologias
               (expandir (first expandibles) expandidas)
            )
         )
         (append expandidas (rest expandibles))
      )
   )
)

(defun pertenece-vacia (clausulas &aux (clausula (first clausulas)))
   (if clausulas
       (or (and (null (first clausula)) (null (second clausula)))
           (pertenece-vacia (rest clausulas))
       )
   )
)

(defun pertenece-vacia-bis (clausulas &aux (clausula (first clausulas)))
   (and clausulas (null (first clausula)) (null (second clausula)))
)

(print 'evaluar-ok)
