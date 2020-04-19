;;; Google Code Jam 2020, Round 1B, Problem 1: Expogo
;;
;; This is suboptimal but works for all cases.
;;
;; We do a recursive search with memoization (caching), and prune
;; impossible regions.
;;
;; Apparently there is also a constructive method, which I suspected
;; but couldn't find.

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (the (integer 0 1000) (read in)))
    (format t "Case #~D: " (+ caseno 1))
    (solve-case in)))

(defun solve-case (in)
  (solve-case-1 (read in) (read in)))

(defun solve-case-1 (x y)
  (let ((min-jumps (floor (log (+ (abs x) (abs y)) 2))))
    (let ((memo (make-hash-table :test #'equal)))
      (do ((k (- min-jumps 1) (+ k 1)))
          ((> k (+ min-jumps 1))
           (format t "IMPOSSIBLE~%"))
        (let ((sol (solve-case-2 (ash 1 k) x y memo)))
          (unless (eq sol 'false)
            (dolist (move (nreverse sol))
              (format t "~C" move))
            (format t "~%")
            (return-from solve-case-1 t)))))))

(defun solve-case-2 (jump x y memo)
  (let ((result (gethash (list jump x y) memo)))
    (or result
        (setf (gethash (list jump x y) memo)
              (cond ((= jump 0)
                     (if (= x y 0) '() 'false))
                    ((>= (+ (abs x) (abs y)) (+ jump jump)) 'false)
                    (t (dolist (dir (list #\W #\E #\N #\S) 'false)
                         (multiple-value-bind (xinc yinc)
                             (ecase dir
                               ((#\W) (values (- jump) 0))
                               ((#\E) (values jump 0))
                               ((#\N) (values 0 jump))
                               ((#\S) (values 0 (- jump))))
                           (let ((sol (solve-case-2
                                       (ash jump -1)
                                       (- x xinc)
                                       (- y yinc)
                                       memo)))
                             (unless (eq sol 'false)
                               (return (cons dir sol))))))))))))
(solve)
