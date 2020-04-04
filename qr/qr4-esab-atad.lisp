;;; Google Code Jam 2020, Qualification Round, Problem 4: esaB ataD

(defun solve (&optional setno)
  (if setno
      (let ((p (sb-ext:run-program "/usr/local/bin/python3" (list "local_testing_tool.py" (format nil "~D" setno))
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
        (N (read i)))
    (catch 'solve-exit
      (dotimes (caseno ncase)
        (solve-case N i o)))))

(defun solve-case (N in out)
  (let ((qi 0)
        (known 0)
        (npairs (ceiling N 2))
        (q (make-array (list 4) :initial-element '())))
    (labels
        ((query (k)
           (incf qi)
           (format out "~D~%" (+ k 1))
           (finish-output out)
           (let ((result (read in)))
             result)))
      (do ()
          ((>= known npairs))
        (when (and (> qi 0) (= (mod qi 10) 0))
          (let* ((invertedp
                  ;; check for inversion
                  (if (or (not (endp (aref q 0)))
                          (not (endp (aref q 3))))
                      (multiple-value-bind (v i)
                          (if (not (endp (aref q 0)))
                              (values 0 (first (aref q 0)))
                              (values 3 (first (aref q 3))))
                        (let ((new (query i)))
                          (/= new (ash v -1))))
                      ;; if we don't have any 0 or 3 pairs,
                      ;; then we cannot distinguish between inversion and swap.
                      ;; Let's assume there was no inversion.
                      (let ((ignore (query 0)))
                        (declare (ignore ignore))
                        nil)))
                 (swappedp
                  ;; check for swap
                  (if (or (not (endp (aref q 1)))
                          (not (endp (aref q 2))))
                      (multiple-value-bind (v i)
                          (if (not (endp (aref q 1)))
                              (values 1 (first (aref q 1)))
                              (values 2 (first (aref q 2))))
                        (let ((new (query i)))
                          (/= new (if invertedp (- 1 (ash v -1)) (ash v -1)))))
                      ;; if we don't have any 1 or 2 pairs,
                      ;; then we cannot detect swaps.
                      ;; But we don't care either, so we can safely assume there was none.
                      (let ((ignore (query 0)))
                        (declare (ignore ignore))
                        nil))))
            (when invertedp
              (rotatef (aref q 0) (aref q 3))
              (rotatef (aref q 1) (aref q 2)))
            (when swappedp
              (rotatef (aref q 1) (aref q 2)))
            ))
        (let* ((a (query known))
               (b (query (- N 1 known)))
               (x (+ (* a 2) b)))
          (push known (aref q x))
          (incf known)))
      (let ((result (make-array (list N))))
        (dotimes (a 2)
          (dotimes (b 2)
            (let ((x (+ (* a 2) b)))
              (dolist (r (aref q x))
                (setf (aref result r) a)
                (setf (aref result (- N 1 r)) b)))))
        (dotimes (i N)
          (format out "~D" (aref result i)))
        (format out "~%")
        (finish-output out)
        (let ((verdict (read-line in)))
          (cond ((string= verdict "N")
                 (throw 'solve-exit nil))
                ((string= verdict "Y"))
                (t (warn "Judge responds with unexpected verdict \"~A\"" verdict))))))))

(solve)
