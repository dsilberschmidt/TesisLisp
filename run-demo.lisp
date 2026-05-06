(load "loader.lisp")

(in-package :tesis)

(load "PROBLEMA/FUENTES.LSP")

(compilar dama1 'objeto-dama1)

(let ((clausulas (symbol-value 'objeto-dama1)))
  (format t "~&DAMA1 internal clauses: ~D~%" (length clausulas)))
