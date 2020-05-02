;;; Google Code Jam 2020, Round 1C, Problem 2: Overrandomized
;;

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (read in))
    (format t "Case #~D: ~A~%" (+ caseno 1) (solve-case in))))

(defun solve-case (in)
  (let ((u (read in))
        (pairs (make-array (list 10000)))
        (charset '()))
    (dotimes (i 10000)
      (let ((qi (read in))
            (ri (string-trim '(#\Space #\Tab #\Newline) (read-line in))))
        (dotimes (k (length ri))
          (unless (member (aref ri k) charset)
            (push (aref ri k) charset)))
        (setf (aref pairs i) (cons qi ri))))
    (solve-case-1 u pairs charset)))

(defun find-zero (pairs charset)
  (first (set-difference
          charset
          (reduce #'union
                  (map 'list #'(lambda (pair) (list (aref (cdr pair) 0)))
                       pairs)))))

(defun solve-case-1 (u pairs charset)
  (declare (ignore u))
  (when (/= (length charset) 10)
    (return-from solve-case-1 "CHARSETERR"))
  (unless (every #'(lambda (c) (and (alpha-char-p c) (upper-case-p c))) charset)
    (return-from solve-case-1 "NOTALLCAPS"))
  (unless (every #'(lambda (pair) (> (length (cdr pair)) 0)) pairs)
    (return-from solve-case-1  "EMPTYSTR"))

  (let ((zero (find-zero pairs charset)))

    ;; Now count leading digits
    (let ((counts (make-array (list 10) :initial-element 0)))
      (dotimes (i 10000)
        (let ((ri (cdr (aref pairs i))))
          (incf (aref counts (position (aref ri 0) charset)))))

      (let ((count-to-digit (pairlis (coerce counts 'list) charset)))
        (let ((sorted-counts (sort counts #'>)))
          (let ((result (make-string 10)))
            (setf (aref result 0) zero)
            (dotimes (i 9)
              (setf (aref result (1+ i))
                    (cdr (assoc (aref sorted-counts i) count-to-digit))))
            result))))))

(solve)
