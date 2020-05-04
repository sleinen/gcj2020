;;; Google Code Jam 2020, Round 1C, Problem 2: Overrandomized
;;;
;;; See https://blog.simon.leinen.ch/2020/05/google-code-jam-2020-in-lispround-1c.html
;;;
;;; This code is supposed to solve the problem for all inputs.  It
;;; always ignores the random queries (Qi), because they aren't
;;; defined in the "difficult" input set.
;;;
;;; The idea is as follows:
;;;
;;; First we find out which symbol stands for the digit 0 (zero).
;;; Since there are no leading zeroes, and the random numbers are
;;; never 0, we know that the first symbols can never represent zero.
;;; So we look for the symbol that never appears as a first digit.
;;;
;;; Next, we look at the counts of how often each of the remaining
;;; nine symbols appears as the first digit.  The most frequently
;;; occurring symbol certainly stands for the digit 1, the next most
;;; for digit 2, and so on.
;;;
;;; This sounds stupid, but it works reliably enough when the input
;;; set is as large as 10000 trials.
;;;
(labels
    ((i2c (i) (code-char (+ i #.(char-code #\A))))
     (c2i (c) (- (char-code c) #.(char-code #\A))))
  (dotimes (caseno (read))
    (format t "Case #~D: " (+ caseno 1))
    (let ((u (read))
          (seen 0)
          (d1-counters (make-array (list 26) :initial-element 0)))
      (dotimes (i 10000)
        ;; Note: Initially, I simply read QI and RI as follows:
        ;;
        ;;   (let ((qi (read)) (ri (read-line))) ...)
        ;;
        ;; However, that caused a "RE" (runtime error) somewhere when
        ;; submitted to the Code Jam platform.  Splitting the lines
        ;; explicitly as below evades that issue.
        ;;
        (let ((line (read-line)))
          (let ((space-pos (position #\Space line)))
            (assert space-pos)
            (assert (not (zerop space-pos)))
            (let ((qi (parse-integer line :start 0 :end space-pos))
                  (ri (subseq line (1+ space-pos))))
              (assert (or (= qi -1) (<= 1 qi (- (expt 10 u) 1))))
              (assert (<= 1 (length ri) u))
              (assert (every #'alpha-char-p ri))
              (assert (every #'upper-case-p ri))
              (incf (aref d1-counters (c2i (char ri 0))))
              (dotimes (k (length ri))
                (setf seen (logior seen (ash 1 (c2i (char ri k))))))))))
      (let ((zero (dotimes (i 26 0)
                    (when (and (logbitp i seen) (zerop (aref d1-counters i)))
                      (return i)))))
        (write-char (i2c zero))
        (let ((sorted-counters (sort (copy-seq d1-counters) #'>)))
          (dotimes (i 9)
            (write-char (i2c (position (aref sorted-counters i) d1-counters)))))))
    (terpri)))
