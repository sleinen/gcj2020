;;; Google Code Jam 2020, Qualification Round, Problem 3: Parenting Partnering Returns

(defun solve (&optional (in *standard-input*))
  (dotimes (caseno (read in))
    (format t "Case #~D: " (1+ caseno))
    (solve-case in)))

(defstruct (graph (:constructor make-graph (n-nodes)))
  (n-nodes)
  (nodes)
  (links))

(defmethod print-object ((g graph) out)
  (print-unreadable-object (g out :type t)
    (dotimes (i (graph-n-nodes g))
      (format out "~A" (or (node-color (graph-node g i)) #\–)))
    (format out " with ~D nodes" (graph-n-nodes g))))

(defstruct (node)
  (graph nil :type graph)
  (index nil :type unsigned-byte)
  (S)
  (E)
  (color)
  (discovered nil)
  (processed nil))

(defmethod print-object ((n node) out)
  (print-unreadable-object (n out :type t)
    (format out "#~D ~D–~D" (node-index n) (node-S n) (node-E n))
    (format out " ~A" (ecase (node-color n)
                        ((#\C) #\C)
                        ((#\J) #\J)
                        ((nil) #\–)))))

(defun graph-node (g i)
  (aref (graph-nodes g) i))

(defun nodes-linked-p (n1 n2)
  (let ((l (graph-links (node-graph n1))))
    (not (zerop (aref l (node-index n1) (node-index n2))))))

(defun create-graph (n-nodes)
  (let ((g (make-graph n-nodes)))
    (setf (graph-nodes g) (make-array (list n-nodes)))
    (setf (graph-links g) (make-array (list n-nodes n-nodes) :element-type 'bit :initial-element 0))
    g))

(defmacro do-nodes ((var graph) &body body)
  (let ((i (gensym (symbol-name :i)))
        (g (gensym (symbol-name :g))))
    `(let ((,g ,graph))
       (dotimes (,i (graph-n-nodes ,g))
         (let ((,var (graph-node ,g ,i)))
           ,@body)))))

(defmacro do-neighbors ((var node) &body body)
  (let ((g (gensym (symbol-name :g)))
        (n1 (gensym (symbol-name :n1))))
    `(let* ((,n1 ,node)
            (,g (node-graph ,n1)))
       (do-nodes (,var ,g)
         (when (and (not (eq ,n1 ,var)) (nodes-linked-p ,n1 ,var))
           ,@body)))))

(defun make-queue ()
  (cons nil '()))

(defun queue-empty-p (q)
  (declare (type cons q))
  (not (endp (cdr q))))

(defun pop-queue (q)
  (declare (type cons q))
  (assert (not (endp (cdr q))))
  (let ((result (first (cdr q))))
    (setf (cdr q) (rest (cdr q)))
    result))

(defparameter default-color #\C)

(defun complement-color (color)
  (case color
    ((#\C) #\J)
    ((#\J) #\C)))

(defun push-queue (v q)
  (setf (cdr q) (cons v (cdr q)))
  v)

(defun bfs (g fun-ne fun-nl fun-edge)
  (let ((q (make-queue)))
    (do-nodes (n1 g)
      (unless (node-discovered n1)
        (push-queue n1 q)
        (setf (node-discovered n1) t)
        (setf (node-color n1) default-color)
        (do ()
            ((not (queue-empty-p q)))
          (let ((v (pop-queue q)))
            (funcall fun-ne v)
            (setf (node-processed v) t)
            (do-neighbors (y v)
              (unless (node-processed y)
                (funcall fun-edge v y))
              (unless (node-discovered y)
                (push-queue y q)
                (setf (node-discovered y) t)))
            (funcall fun-nl v)))))))

(defun compute-overlap (g)
  (labels
      ((overlapp (ni nj)
         (or (overlapp-1 ni nj)
             (overlapp-1 nj ni)))
       (overlapp-1 (ni nj)
         (if (<= (node-S ni) (node-S nj))
             (> (node-E ni) (node-S nj))
             (and (< (node-E ni) (node-E nj))
                  (< (node-S ni) (node-E nj))))))
    ;; detect overlaps
    (do-nodes (n1 g)
      (do-nodes (n2 g)
        (unless (eq n1 n2)
          (setf (aref (graph-links g) (node-index n1) (node-index n2))
                (if (overlapp n1 n2) 1 0)))))))

(defun solve-case (in)
  (let* ((n-nodes (read in))
         (g (create-graph n-nodes)))
    ;; read chores
    (dotimes (i n-nodes)
      (setf (aref (graph-nodes g) i)
            (make-node
             :graph g
             :index i
             :S (read in)
             :E (read in))))
    ;; compute links
    (compute-overlap g)
    ;; color graph
    (bfs g
         #'(lambda (n) (declare (ignore n)))
         #'(lambda (n) (declare (ignore n)))
         #'(lambda (n1 n2)
             (when (eq (node-color n1) (node-color n2))
                 (format t "IMPOSSIBLE~%")
                 (return-from solve-case (values)))
             (unless (node-color n2)
               (setf (node-color n2)
                     (if (node-color n1) (complement-color (node-color n1)) default-color)))))
    (dotimes (i n-nodes)
      (format t "~A" (node-color (graph-node g i))))
    (format t "~%")))

(solve)
