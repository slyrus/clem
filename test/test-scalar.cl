
(in-package :matrix)


(defun divvy (a k q fit)
  (declare (ignore fit))
  (map-row a k #'(lambda (x) (/ x q))))

;(defun moose (a k q fit)
;(declare (ignore k q fit))
;  (matrix::print-matrix a))
;(matrix::def-scalar-op "foo" #'moose)

;(defparameter *scalar-ops*
;  (list (list "mult2" #'*)
;	(list "divide2" #'/)
;	(list "float-divide2" #'util::float-divide)))
;
;(mapcar #'(lambda (x)
;	    (matrix::def-scalar-ops (car x) (cadr x)))
;	*scalar-ops*)

(let ((x (array->matrix #2A((213 43 56)(29 104 191)(2 45 112)))))
  (print 'foo)
  (matrix::print-matrix x)
  (matrix::scalar-double-float-divide-row x 1 3)
  (matrix::print-matrix x)
  (print 'moose))
