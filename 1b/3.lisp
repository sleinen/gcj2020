;;; Google Code Jam 2020, Round 1B, Problem 3: Join The Ranks

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (the (integer 0 1000) (read in)))
    (format t "Case #~D: " (+ caseno 1))
    (solve-case in)))

(defun solve-case (in)
  (solve-case-1 (read in) (read in)))

(defun solve-case-1 (r s)
  (let ((moves #((3 . 2) (2 . 1))))
    (format t "~D~%" (length moves))
    (dotimes (k (length moves))
      (let ((move (elt moves k)))
        (format t "~D ~D~%" (car move) (cdr move))))))

(solve)

