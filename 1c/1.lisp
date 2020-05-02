;;; Google Code Jam 2020, Round 1C, Problem 1: Overexcited Fan
;;

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (the (integer 0 1000) (read in)))
    (format t "Case #~D: " (+ caseno 1))
    (solve-case in)))

(defun dir-xy-offset (c)
  (ecase c
    ((#\N) (values  0  1))
    ((#\S) (values  0 -1))
    ((#\W) (values -1  0))
    ((#\E) (values  1  0))
    ((#\*) (values  0  0))))            ;Stay where we are

(defun dir-x-offset (c)
  (multiple-value-bind (x y) (dir-xy-offset c) (declare (ignore y)) x))
(defun dir-y-offset (c)
  (multiple-value-bind (x y) (dir-xy-offset c) (declare (ignore x)) y))

(defun solve-case (in)
  (format t "~A~%"
          (solve-case-1
           (read in)
           (read in)
           (map 'list #'identity (read-line in))
           0
           nil)))

(defvar *debug* nil)

(defun warnd (format &rest args)
  (when *debug* (apply #'warn format args)))

(defun solve-case-1 (x y seq steps best-so-far)
  (cond ((= x y 0)
         (warnd "BRANCH CLOSED-IN")
         (if best-so-far (min best-so-far steps) steps))
        ((and best-so-far (> steps best-so-far)) ;We can't get any better
         (warnd "BRANCH BEST")
         best-so-far)
        ((<= (+ (abs x) (abs y)) steps) ;We can get there in time
         (warnd "BRANCH CLOSE-ENOUGH")
         (let ((new-solution steps))
           (let ((new-best (if best-so-far (min best-so-far new-solution) new-solution)))
             (if (endp seq)
                 new-best
                 (multiple-value-bind (dx dy)
                     (dir-xy-offset (first seq))
                   (solve-case-1 (+ x dx) (+ y dy) (rest seq) (1+ steps) new-best))))))
        ((endp seq)
         (warnd "BRANCH FINISHED-SEQ")
         (or best-so-far 'IMPOSSIBLE))
        (t
         (warnd "BRANCH ELSE")
         (multiple-value-bind (dx dy)
             (dir-xy-offset (first seq))
           (solve-case-1 (+ x dx) (+ y dy) (rest seq) (1+ steps) best-so-far)))
        ))

;(trace solve-case-1)
(solve)
