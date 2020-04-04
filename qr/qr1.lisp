;;; Google Code Jam 2020, Qualification Round, Problem 1: Vestigium

(defun solve (&optional (in *standard-input*))
  (let ((ncases (read in)))
    (dotimes (caseno ncases)
      (solve-case caseno in))))

(defun solve-case (caseno in)
  (let ((N (read in)))
    (let ((M (make-array (list N N))))
      ;; read array
      (dotimes (i N)
	(dotimes (j N)
	  (let ((Mij (read)))
	    (setf (aref M i j) Mij))))
      (let ((trace 0)
	    (badrows 0)
	    (badcols 0))
	(dotimes (a N)
	  (let ((row-seen (make-array (list N) :element-type 'bit :initial-element 0))
		(col-seen (make-array (list N) :element-type 'bit :initial-element 0))
		(row-duplicate-p nil)
		(col-duplicate-p nil))
	    (incf trace (aref M a a))
	    (dotimes (b N)
	      (let ((c1 (aref M a b))
		    (c2 (aref M b a)))
		(if (not (zerop (aref row-seen (1- c1))))
		    (setq row-duplicate-p t)
		    (setf (aref row-seen (1- c1)) 1))
		(if (not (zerop (aref col-seen (1- c2))))
		    (setq col-duplicate-p t)
		    (setf (aref col-seen (1- c2)) 1))))
	    (when row-duplicate-p (incf badrows))
	    (when col-duplicate-p (incf badcols))))
	(format t "Case #~D: ~D ~D ~D~%"
		(+ caseno 1)
		trace badrows badcols)))))

(solve)