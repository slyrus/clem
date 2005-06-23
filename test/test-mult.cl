
(in-package :clem-test)

(defparameter *mult-test-matrix-size* 32)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest args)
    (intern (string-upcase (apply #'concatenate 'string args)))))

(defmacro def-mult-test (type-1 val-1 type-2 val-2)
  (let ((funcname (symbolicate "test/mat-mult/" (symbol-name type-1)
			       "/" (symbol-name type-2)))
	(m1 (symbolicate (symbol-name type-1) "-matrix"))
	(m2 (symbolicate (symbol-name type-2) "-matrix")))
    `(defun ,funcname (&key (rows *mult-test-matrix-size*)
		       (cols *mult-test-matrix-size*))
       (let ((m (make-instance ',m1 :rows rows :cols cols
			       :initial-element ,val-1))
	     (n (make-instance ',m2 :rows cols :cols rows
			       :initial-element ,val-2)))
	 (let ((p (time (clem::mat-mult m n))))
	   p)))))

(def-mult-test double-float 1.25d0 double-float (coerce pi 'double-float))
(def-mult-test single-float 1.25s0 single-float (coerce pi 'single-float))

(defun mult-test-2 ()
  (let ((m2 (clem::array->double-float-matrix
	     #2A((1d0 2d0 3d0 4d0) (1d0 2d0 3d0 4d0) (1d0 2d0 3d0 4d0) (1d0 2d0 3d0 4d0)))))
    (clem::mat-mult m2 m2)))