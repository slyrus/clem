
(in-package :clem)
  
(defmacro def-matrix-mref (type)
  (let ((element-type (element-type (find-class `,type))))
    `(progn
       (defmethod mref ((m ,type) (row fixnum) (col fixnum))
	 (with-typed-matrix-vals (m ,element-type t a)
	   (aref a row col)))
       
       (defmethod (setf mref) (v (m ,type) (row fixnum) (col fixnum))
	 (with-typed-matrix-vals (m ,element-type t a)
	   (setf (aref a row col) v))))))

(macrolet ((frob (type)
	     `(def-matrix-mref ,type)))
  (frob double-float-matrix)
  (frob single-float-matrix)
  (frob ub8-matrix)
  (frob ub16-matrix)
  (frob ub32-matrix)
  (frob sb8-matrix)
  (frob sb16-matrix)
  (frob sb32-matrix)
  (frob fixnum-matrix)
  (frob bit-matrix))
