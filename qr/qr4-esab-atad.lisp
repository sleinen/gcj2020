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
  (let ((ncase (read i)))
    (catch 'solve-exit
      (dotimes (caseno ncase)
	(let* ((B (read i)))
	  (solve-case B i o))))))

(defun solve-case (B in out)
  (format out "1110110000~%")
  (finish-output out)
  (let ((verdict (read-line in)))
    (when (string= verdict "N")
      (warn "lost verdict"))
    (when (string= verdict "Y")
      (throw 'solve-exit nil))))

(solve 0)
