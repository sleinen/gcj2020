;;; Google Code Jam 2020, Round 1C, Problem 1: Overexcited Fan
;;

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (the (integer 0 1000) (read in)))
    (format t "Case #~D: ~A~%" (+ caseno 1) (solve-case in))))

(defun solve-case (in)
  (solve-case-1 (read in) (read in) (coerce (read-line in) 'list) 0))

(defun solve-case-1 (x y seq steps)
  (labels
      ((dir-xy-offset (c)
         (values (ecase c ((#\N) 0) ((#\S)  0) ((#\W) -1) ((#\E) 1))
                 (ecase c ((#\N) 1) ((#\S) -1) ((#\W)  0) ((#\E) 0)))))
    (cond ((<= (+ (abs x) (abs y)) steps) steps)
          ((endp seq) 'IMPOSSIBLE)
          (t (multiple-value-bind (dx dy)
                 (dir-xy-offset (first seq))
               (solve-case-1 (+ x dx) (+ y dy) (rest seq) (1+ steps)))))))

(solve)
