;;; Google Code Jam 2020, Round 1C, Problem 2: Overrandomized
;;

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (read in))
    (format t "Case #~D: ~A~%" (+ caseno 1) (solve-case in))))

(defun solve-case (in)
  (let ((u (read in))
        (entries (make-array (list 10000)))
        (charset '()))
    (dotimes (i 10000)
      (let ((qi (read in))
            (ri (string-trim '(#\Space #\Tab #\Newline) (read-line in))))
        (declare (ignore qi))
        (dotimes (k (length ri))
          (unless (member (char ri k) charset)
            (push (char ri k) charset)))
        (setf (aref entries i) (char ri 0))))
    (solve-case-1 u entries charset)))

(defun solve-case-1 (u entries charset)
  (declare (ignore u))
  (when (/= (length charset) 10)
    (return-from solve-case-1 "CHARSETERR"))
  (unless (every #'(lambda (c) (and (alpha-char-p c) (upper-case-p c))) charset)
    (return-from solve-case-1 "NOTALLCAPS"))

  ;; Now count leading digits
  (let ((counts (make-array (list 10) :initial-element 0)))
    (dotimes (i 10000)
      (let ((ri (aref entries i)))
        (incf (aref counts (position ri charset)))))

    (let ((count-to-digit (pairlis (coerce counts 'list) charset)))
      (let ((sorted-counts (sort counts #'>)))
        (let ((result (make-string 10)))
          (setf (char result 0)
                (first (set-difference
                        charset
                        (reduce #'union
                                (map 'list #'(lambda (entry) (list entry))
                                     entries)))))
          (dotimes (i 9)
            (setf (char result (1+ i))
                  (cdr (assoc (aref sorted-counts i) count-to-digit))))
          result)))))

(solve)
