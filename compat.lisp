;;;; compat.lisp — shim mínimo para dialecto pre-CL

;; --- Lector: [a b c] => (a b c)
(set-macro-character #\[
  (lambda (s c) (declare (ignore c))
    (let ((lst (read-delimited-list #\] s t))) lst)))
(set-macro-character #\] (get-macro-character #\)))

;; --- Azúcares y utilidades del dialecto

;; DE: mapea &OPT (cualquiera sea el paquete) -> &OPTIONAL
(defmacro de (name args &body body)
  (labels ((fix (lst)
             (if (endp lst)
                 nil
                 (let ((h (first lst)) (t (cdr lst)))
                   (cond
                     ((and (symbolp h)
                           (string= (symbol-name h) "&OPT"))
                      (cons '&optional (fix t)))
                     (t (cons h (fix t))))))))
    `(defun ,name ,(fix args) ,@body)))

(defmacro on  (var) `(setf ,var t))
(defmacro off (var) `(setf ,var nil))

(defun <> (a b) (not (equal a b)))
(defun sub1 (x) (1- x))
(defun restn (lst &optional (n 1)) (nthcdr n lst))
(defun append+ (&rest lists) (apply #'append lists))
(defun prin2 (x) (princ x))
(defun print2 (x) (prog1 (princ x) (terpri)))

;; selectq: como CASE con claves símbolo
(defmacro selectq (key &body clauses)
  `(case ,key
     ,@(mapcar (lambda (cl)
                 (destructuring-bind (k &rest forms) cl
                   (let ((kk (if (and (consp k) (eq (first k) 'quote))
                                 (second k) k)))
                     (if (listp kk)
                         `((,@kk) ,@forms)
                         `((,kk) ,@forms)))))
               clauses)))

;; ---------------- Tests del shim
(defun run-shim-tests ()
  (assert (equal (with-input-from-string (s "[a b (c d)]") (read s))
                 '(a b (c d))))
  (de add (x y &opt z) (+ x y (or z 0)))
  (assert (= (add 1 2) 3))
  (assert (= (add 1 2 3) 6))
  (let ((r nil))
    (selectq 'foo
      (bar (setf r :bad))
      (foo (setf r :ok)))
    (assert (eq r :ok)))
  (let ((flag nil)) (on flag) (assert flag) (off flag) (assert (null flag)))
  (assert (<> 'a 'b))
  (assert (not (<> 'a 'a)))
  (assert (= (sub1 5) 4))
  (assert (equal (append+ '(1 2) '(3) '()) '(1 2 3)))
  (prin2 :ok) (print2 :ok)
  (format t "~&compat OK~%")
  :ok)
