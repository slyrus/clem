
(in-package :clem)

(defclass n-d-matrix (matrix)
  ((m :accessor matrix-vals)
   (dimensions :accessor matrix-dimensions :initarg :dimensions :initform '(1) :type (or list null))
   (initial-element :accessor initial-element :initarg :initial-element :initform 0d0)
   (adjustable :accessor adjustable :initarg :adjustable :initform nil)
   (resizeable :accessor resizable :initform nil))
  (:metaclass standard-matrix-class)
  (:element-type double-float)
  (:val-format "~4,9F")
  (:minval nil)
  (:maxval nil))

(defclass n-d-double-float-matrix (n-d-matrix double-float-matrix)
  ((dimensions :accessor matrix-dimensions :initarg :dimensions :initform '(1) :type (or list null)))
  (:metaclass standard-matrix-class)
  (:element-type double-float))

(defmethod allocate-matrix-vals ((object n-d-matrix) &key (dimensions '(1)) adjustable (initial-element 0))
  (setf (slot-value object 'm)
	(make-array dimensions
		    :adjustable adjustable
		    :initial-element (coerce initial-element (element-type (class-of object)))
		    :element-type (element-type (class-of object)))))

(defmethod shared-initialize :after
    ((object n-d-matrix) slot-names &rest initargs &key rows cols adjustable initial-element)
  (declare (ignore slot-names initargs rows cols adjustable initial-element))
  (apply #'allocate-matrix-vals object
         (append
          (list :dimensions (slot-value object 'dimensions))
          (list :adjustable (slot-value object 'adjustable))
          (when (slot-value object 'initial-element)
            (list :initial-element (slot-value object 'initial-element))))))

;;; test code

(defmethod transpose ((m n-d-matrix))
  (destructuring-bind (rows cols three) (dim m)
    (declare (type fixnum rows cols))
    (let ((tr (make-instance 'n-d-matrix :dimensions (dim m))))
      (with-typed-mref (m double-float)
        (with-typed-mref (tr double-float)
          (dotimes (i rows)
            (dotimes (j cols)
              (dotimes (k three)
                (setf (mref tr j i k) (mref m i j k)))))))
      tr)))

