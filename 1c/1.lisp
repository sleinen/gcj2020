;;; Google Code Jam 2020, Round 1C, Problem 1: Overexcited Fan
;;
(labels
    ((solve (x y seq steps)
       (cond ((<= (+ (abs x) (abs y)) steps) steps)
             ((= steps (length seq)) 'IMPOSSIBLE)
             (t (solve
                 (+ x (case (char seq steps) (#\W -1) (#\E 1) (otherwise 0)))
                 (+ y (case (char seq steps) (#\N 1) (#\S -1) (otherwise 0)))
                 seq
                 (1+ steps))))))
  (dotimes (caseno (read))
    (format t "Case #~D: ~A~%"
            (+ caseno 1)
            (solve (read) (read) (read-line) 0))))
