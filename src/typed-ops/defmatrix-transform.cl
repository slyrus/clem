;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-transform.cl
;;; author: cyrus harmon
;;; time-stamp: Thu Jul  7 09:21:21 2005
;;;

(in-package :clem)
  
(defmacro def-matrix-transform (type-1 type-2 transform-type)
  (let ((element-type-1 (element-type (find-class `,type-1)))
	(element-type-2 (element-type (find-class `,type-2)))
	(transform-element-type (element-type (find-class `,transform-type))))
    (let ((zero (coerce 0 `,transform-element-type))
	  (one (coerce 1 `,transform-element-type)))
      `(progn
	 (defmethod %transform-matrix ((m ,type-1) (n ,type-2) (xfrm ,transform-type)
				      &key (background ,zero))
	   (let ((mr (rows m)) (mc (cols m))
		 (nr (rows n)) (nc (cols n)))
	     (declare (type fixnum mr mc nr nc)
		      (optimize (speed 3) (safety 0)))
	     (let  ((inv-xfrm (clem::invert-matrix xfrm))
		    (coord1 (make-instance ',transform-type :rows 3 :cols 1))
		    (coord2 (make-instance ',transform-type :rows 3 :cols 1)))
	       (let ((a (clem::matrix-vals m))
		     (b (clem::matrix-vals n))
		     (c (clem::matrix-vals coord1))
		     (d (clem::matrix-vals coord2)))
		 (declare (type (simple-array ,element-type-1 (* *)) a)
			  (type (simple-array ,element-type-2 (* *)) b)
			  (type (simple-array ,transform-element-type (* *)) c d))
		 (setf (aref c 2 0) ,one)
		 (dotimes (i nr)
		   (declare (type fixnum i))
		   (dotimes (j nc)
		     (declare (type fixnum j))
		     (setf (aref d 0 0) ,zero
			   (aref d 1 0) ,zero
			   (aref d 2 0) ,zero)
		     (setf (aref c 0 0) (coerce i ',transform-element-type)
			   (aref c 1 0) (coerce j ',transform-element-type))
		     (clem::mat-mult3 inv-xfrm coord1 coord2)
		     (let ((oldx (the fixnum (round (aref d 0 0))))
			   (oldy (the fixnum (round (aref d 1 0)))))
		       (declare (type fixnum oldx oldy))
		       (if (and (< -1 oldx mr)
				(< -1 oldy mc))
			   (setf (aref b i j) (aref a oldx oldy))
			   (setf (aref b i j) background))))))))
	   n)))))

(macrolet ((frob (type-1 type-2 type-3)
	     `(def-matrix-transform ,type-1 ,type-2 ,type-3)))
  (frob double-float-matrix double-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix single-float-matrix)
  (frob ub8-matrix ub8-matrix double-float-matrix)
  (frob ub16-matrix ub16-matrix double-float-matrix)
  (frob ub32-matrix ub32-matrix double-float-matrix)
  (frob sb8-matrix sb8-matrix double-float-matrix)
  (frob sb16-matrix sb16-matrix double-float-matrix)
  (frob sb32-matrix sb32-matrix double-float-matrix)
  (frob fixnum-matrix fixnum-matrix double-float-matrix)
  (frob bit-matrix bit-matrix double-float-matrix))

