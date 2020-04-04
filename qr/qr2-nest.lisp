;;; Google Code Jam 2020, Qualification Round, Problem 2: Nesting Depth

(defun solve (&optional (in *standard-input*))
  (let ((ncases (read in)))
    (dotimes (caseno ncases)
      (solve-case caseno in))))

(defun fix-parens (now need)
  (do () ((>= now need)) (format t "(") (incf now))
  (do () ((<= now need)) (format t ")") (decf now))
  now)

(defun parenthesize (S)
  (let ((nest 0))
    (do ((k 0 (1+ k)))
        ((>= k (length S)))
      (let ((dig (parse-integer (subseq S k (1+ k)))))
        (setq nest (fix-parens nest dig))
        (format t "~D" dig)))
    (fix-parens nest 0)))

(defun solve-case (caseno in)
  (let ((S (read-line in)))
    (format t "Case #~D: " (+ caseno 1))
    (parenthesize S)
    (format t "~%")))

(solve)
