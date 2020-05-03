(defun test-2 (&optional (base-pathname "2x") (n 10) (u 2) (garblep nil))
  (let ((in-filename (merge-pathnames base-pathname (make-pathname :type "in")))
        (out-filename (merge-pathnames base-pathname (make-pathname :type "out"))))
    (with-open-file (in in-filename
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
      (with-open-file (out out-filename
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
        (let ((max (1- (expt 10 u))))
          (format in "~D~%" n)
          (dotimes (caseno n)
            (let ((charset (random-charset)))
              (format in "~D~%" u)
              (dotimes (i 10000)
                (let ((r1 (1+ (random max))))
                  (let ((r2 (1+ (random r1))))
                    (let ((decimal (format nil "~D" r2)))
                      (format in "~D ~A~%" (if garblep -1 r1) (encode-decimal decimal charset))))))
              (format out "Case #~D: ~A~%" (1+ caseno) charset))))))))

(defun random-charset (&optional (n 10) (base "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (let ((result (make-string n :initial-element #\?)))
    (dotimes (k n result)
      (loop
        (let ((r (char base (random (length base)))))
          (unless (find r result)
            (setf (char result k) r)
            (return)))))))

(defun encode-decimal (dec charset)
  (let ((result (make-string (length dec) :initial-element #\?)))
    (dotimes (k (length dec))
      (let ((digit (parse-integer dec :start k :end (+ k 1))))
        (setf (aref result k) (aref charset digit))))
    result))
