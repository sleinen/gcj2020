;;; Google Code Jam 2020, Round 1C, Problem 2: Overrandomized
;;

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (the (integer 0 1000) (read in)))
    (format t "Case #~D: ~A~&" (+ caseno 1) (solve-case in))))

(defun solve-case (in)
  (let ((u (read in)))
    (let ((pairs (make-array (list 10000))))
      (let ((charset '()))
        (dotimes (i 10000)
          (let ((qi (read in)) (ri (string-trim '(#\Space #\Tab #\Newline) (read-line in))))
            (dotimes (k (length ri))
              (unless (member (aref ri k) charset)
                (push (aref ri k) charset)))
            (setf (aref pairs i) (cons qi ri))))
        (solve-case-1 u pairs charset)))))

(defun find-zero (u pairs charset)
  (declare (ignore u))
  (let ((zero-candidates charset))
    (dotimes (i 10000)
      (let ((first-digit (aref (cdr (aref pairs i)) 0)))
        (when (member first-digit zero-candidates)
          (setq zero-candidates
                (set-difference zero-candidates (list first-digit))))))
    ;; This can only happen when there are no zeroes in the dataset.
    ;; Highly unlikely!
    (when (endp zero-candidates)
      (return-from find-zero (first charset)))
    ;; This can happen when a non-zero digit fails to occur as a leading digit.
    ;; Also highly unlikely!
    ;; (unless (endp (rest zero-candidates))
    ;;   (return-from find-zero (first charset)))
    (first zero-candidates)))

(defun solve-case-1 (u pairs charset)
  (when (/= (length charset) 10)
    (return-from solve-case-1 "CHARSETERR"))
  (unless (every #'(lambda (c) (and (alpha-char-p c) (upper-case-p c))) charset)
    (return-from solve-case-1 "NOTALLCAPS"))
  (unless (every #'(lambda (pair) (> (length (cdr pair)) 0)) pairs)
    (return-from solve-case-1  "EMPTYSTR"))

  (let ((zero (find-zero u pairs charset)))
    ;; Pad all Qi with leading zeroes
    (dotimes (i 10000)
      (let ((pair (aref pairs i)))
        (when (< (length (cdr pair)) u)
          (setf (cdr pair)
                (concatenate 'string
                             (make-string (- u (length (cdr pair)))
                                          :initial-element zero)
                             (cdr pair)))))))

  ;; Now count leading digits
  (let ((counts (make-array (list 10) :initial-element 0)))
    (dotimes (i 10000)
      (let ((pair (aref pairs i)))
        (let ((ri (cdr pair)))
          (dotimes (k 1)
            (incf (aref counts (position (aref ri k) charset)))))))

    (let ((count-to-digit (pairlis (coerce counts 'list) charset)))
      (let ((sorted-counts (sort counts #'>)))
        (let ((result (make-string 10)))
          (dotimes (i 10)
            (setf (aref result i)
                  (cdr (assoc (aref sorted-counts i) count-to-digit))))
          result)))))

(solve)
