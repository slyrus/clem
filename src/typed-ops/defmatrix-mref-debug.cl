
(in-package :clem)
  
(declaim (inline mref-df))
(defmacro def-matrix-mref (type)
  (let ((element-type (element-type (find-class `,type))))
    `(progn
       (defun mref-df (m row col)
	 (with-typed-matrix-vals (m ,element-type t a)
	   (aref a row col)))
       
       (defun (setf mref-df) (v m row col)
	 (with-typed-matrix-vals (m ,element-type t a)
	   (setf (aref a row col) v))))))

(macrolet ((frob (type)
	     `(def-matrix-mref ,type)))
  (frob double-float-matrix))
