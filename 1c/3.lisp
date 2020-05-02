;;; Google Code Jam 2020, Round 1C, Problem 3: Oversized Pancake Choppers
;;

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (the (integer 0 1000) (read in)))
    (format t "Case #~D: ~A~&" (+ caseno 1) (solve-case in))))

(defun solve-case (in)
  (let ((n (read in))
        (d (read in)))
    (let ((slices (make-array (list n))))
      (dotimes (i n)
        (setf (aref slices i) (read in)))
      (solve-case-1 n d slices))))

(defun solve-case-1 (n d slices)
  (let ((size-counts (make-hash-table))
        (sizes (remove-duplicates slices)))
    (dotimes (i (length slices))
      (setf (gethash (aref slices i) size-counts)
            (+ 1 (gethash (aref slices i) size-counts 0))))
    (case d
      ((2)
       (cond ((some #'(lambda (size) (>= (gethash size size-counts) 2)) sizes) 0)
             (t 1)))
      ((3)
       (cond ((some #'(lambda (size) (>= (gethash size size-counts) 3)) sizes) 0)
             ((some #'(lambda (size)
                        (and (>= (gethash size size-counts) 2)
                             (some #'(lambda (s2) (> s2 size)) sizes)))
                    sizes)
              1)
             ((some #'(lambda (size) (gethash (* size 2) size-counts)) sizes) 1)
             (t 2)))
      (otherwise (1- d)))))

(solve)
