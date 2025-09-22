(defun run-shim-tests ()
  ;; [] lector
  (assert (equal (with-input-from-string (s "[a b (c d)]") (read s))
                 '(a b (c d))))
  ;; de + &opt
  (de add (x y &opt z) (+ x y (or z 0)))
  (assert (= (add 1 2) 3))
  (assert (= (add 1 2 3) 6))
  ;; selectq
  (let ((r nil))
    (selectq 'foo
      ('bar (setf r :bad))
      ('foo (setf r :ok)))
    (assert (eq r :ok)))
  ;; on/off
  (let ((*flag* nil)) (on *flag*) (assert *flag*) (off *flag*) (assert (null *flag*)))
  ;; <> (not equal)
  (assert (<> 'a 'b))
  (assert (not (<> 'a 'a)))
  ;; rest con n
  (assert (equal (rest '(1 2 3 4) 2) '(3 4)))
  ;; sub1, append+, prin2/print2 (solo que no exploten)
  (assert (= (sub1 5) 4))
  (assert (equal (append+ '(1 2) '(3) '()) '(1 2 3)))
  (prin2 :ok) (print2 :ok)
  (format t "~&compat OK~%") :ok)
