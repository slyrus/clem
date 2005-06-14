
(in-package :clem-test)

(defparameter dfa (make-array '(256 256) :element-type 'double-float :initial-element 2d0))

(defun array-test-1 (a)
  (declare (type (simple-array double-float (* *)) a))
  (destructuring-bind (r c) (array-dimensions a)
    (dotimes (i r)
      (dotimes (j c)
	(setf (aref a i j) (* (aref a i j) 2.0d0))))))