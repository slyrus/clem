;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-square.cl
;;; author: cyrus harmon
;;;

(in-package :clem)
  
(defmacro def-matrix-square (matrix-type)
  (let ((element-type (element-type (find-class `,matrix-type))))
    `(progn
       (defmethod mat-square! ((u ,matrix-type))
	 (destructuring-bind (rows cols) (mapcar #'1- (dim u))
	   (with-map-range u ,element-type 0 rows 0 cols (a i j)
	     (let ((val (aref a i j)))
	       (declare (type ,element-type val))
	       (setf (aref a i j) (* val val)))))))))

(macrolet ((frob (type-1)
	     `(def-matrix-square ,type-1)))
  (frob double-float-matrix))
