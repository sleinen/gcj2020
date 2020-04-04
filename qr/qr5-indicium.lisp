;;; Google Code Jam 2020, Qualification Round, Problem 5: Indicium

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (read in))
    (format t "Case #~D: " (+ caseno 1))
    (solve-case in)))

(defun latin (n root)
  (let ((a (make-array (list n n))))
    (dotimes (i n a)
      (dotimes (j n)
        (setf (aref a i (- n j 1))
              (mod (+ i j root) n))))))

(defun out-square (a)
  (let ((n (array-dimension a 0)))
    (dotimes (i n)
      (dotimes (j n)
        (format t "~D " (1+ (aref a i j))))
      (format t "~%"))))

(defun solve-case (in)
  (solve-case-1 (read in) (read in)))

(defun solve-case-1 (N K)
  (if (< K N)
      (format t "IMPOSSIBLE~%")
      (multiple-value-bind (div rem)
          (truncate K N)
        (if (zerop rem)
            (let ((a (latin N div)))
              (format t "POSSIBLE~%")
              (out-square a))
            (format t "IMPOSSIBLE~%")))))

(solve)
