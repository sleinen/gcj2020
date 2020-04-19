;;; Google Code Jam 2020, Round 1B, Problem 2: Bildfolded Bullseye
;;
;; I got as far as finding a few points near the boundary of the disk,
;; but failed to implement the remainder of the search.
;;
;; This method might get into trouble when it initially hits the disk
;; at a "bad" point, i.e. on a lower/upper/left/right extreme.  In
;; this case, my horizontal/vertical binary search would end up with
;; four points collapsed into two.  Well, maybe that is not so bad
;; because in this case the center of the circle would be right in the
;; middle between the two points...
;;
;; In the end I ran out of time with the rest of the method
;; (computing/approximating the center given a few points on the
;; boundary).  I just managed to implement the "stupid" variant that
;; can solve the easiest data set, and grappled the meager 3 points
;; for that.  (It must be said that those 3 points improved my rank
;; from the 3000s to the (low) 2000s :-)

(defun solve (&optional setno)
  (if setno
      (let ((p (sb-ext:run-program "/usr/local/bin/python3" (list "testing_tool.py" (format nil "~D" setno))
                                   :input :stream :output :stream :wait nil)))
        (assert p)
        (unwind-protect
             (solve-with-streams
              (sb-ext:process-output p)
              (sb-ext:process-input p))
          (sb-ext:process-close p)))
      (solve-with-streams *standard-input* *standard-output*)))

(defun solve-with-streams (i o)
  (let ((ncase (read i))
        (a (read i))
        (b (read i)))
    (catch 'solve-exit
      (dotimes (caseno ncase)
        (solve-case-stupid a b i o)))))

(defconstant xmin -1000000000)
(defconstant xmax 1000000000)
(defconstant ymin -1000000000)
(defconstant ymax 1000000000)

(defun solve-case-stupid (a b i o)
  (do ((y -5 (+ y 1)))
      ((> y 5))
    (do ((x -5 (+ x 1)))
        ((> x 5))
      (format o "~D ~D~%" x y)
      (finish-output o)
      (let ((response (read-line i)))
        (when (string= response "CENTER") (return-from solve-case-stupid t))))))

(defun solve-case (a b i o)
  ;; First, let's find an arbitrary spot on the circle as fast as possible
  (do ((y (+ ymin a) (+ y a)))
      ((> y (- ymax a)))
    (do ((x (+ xmin a) (+ x a)))
        ((> x (- xmax a)))
      (format o "~D ~D~%" x y)
      (finish-output o)
      (let ((response (read-line i)))
        (cond ((string= response "CENTER") (return-from solve-case t)) ;we're done!
              ((string= response "HIT")
               (warn "HIT: ~D ~D" x y)
               (return-from solve-case (solve-case-1 b i o x y)))
              ((string= response "MISS"))
              (t (error "Unexpected answer")))))))

(defun solve-case-1 (b i o x y)
  (let ((in-vec (vector x x y y)))
    (do ((inc b (ash inc -1)))
        ((= inc 0) (solve-case-2 i o in-vec x y))
      (dotimes (dir 4)
        (multiple-value-bind (vary new-x new-y)
            (ecase dir
              ((0) (values (max xmin (- (aref in-vec 0) inc)) (max xmin (- (aref in-vec 0) inc)) 0))
              ((1) (values (min xmax (+ (aref in-vec 1) inc)) (min xmax (+ (aref in-vec 1) inc)) 0))
              ((2) (values (max ymin (- (aref in-vec 2) inc)) 0 (max ymin (- (aref in-vec 2) inc))))
              ((3) (values (min ymax (+ (aref in-vec 3) inc)) 0 (min ymax (+ (aref in-vec 3) inc)))))
          (format o "~D ~D~%" new-x new-y)
          (finish-output o)
          (let ((response (read-line i)))
            (warn "  ~D ~D => ~A" new-x new-y response)
            (cond ((string= response "CENTER") (return-from solve-case-1 t))
                  ((string= response "HIT") (setf (aref in-vec dir) vary)))))))))

(defun solve-case-2 (i o in-vec x y)
  (guess-center i o
                (aref in-vec 0) y
                (aref in-vec 1) y
                x (aref in-vec 2)
                x (aref in-vec 3)))

(defun guess-center (i o x1 y1 x2 y2 x3 y3 x4 y4)
  (error "To Be Implemented"))

(setq *trace-output* *error-output*)

(trace solve-case-1)

(solve)
