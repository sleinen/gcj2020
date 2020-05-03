;;; Google Code Jam 2020, Round 1C, Problem 1: Overexcited Fan
;;
(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (read in))
    (format t "Case #~D: ~A~%"
            (+ caseno 1)
            (solve-case-1 (read in) (read in) (read-line in) 0))))

(defun solve-case-1 (x y seq steps)
  (cond ((<= (+ (abs x) (abs y)) steps) steps)
        ((= steps (length seq)) 'IMPOSSIBLE)
        (t (solve-case-1
            (+ x (case (char seq steps) (#\W -1) (#\E 1) (otherwise 0)))
            (+ y (case (char seq steps) (#\N 1) (#\S -1) (otherwise 0)))
            seq
            (1+ steps)))))

(solve)
