
;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: defmatrix-min-max.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(defmacro def-matrix-min-max (type)
  (let ((element-type (element-type (find-class `,type)))
	(accumulator-element-type (element-type (find-class `,type))))
    `(progn
       (defmethod min-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
         (let ((acc (coerce (aref (matrix-vals m) startr startc) ',accumulator-element-type)))
           (declare (type ,accumulator-element-type acc))
           (with-map-range m ,element-type startr endr startc endc (a i j)
             (when (< (aref a i j) acc)
               (setf acc (aref a i j))))
           acc))
       
       (defmethod max-range ((m ,type) (startr fixnum) (endr fixnum) (startc fixnum) (endc fixnum))
         (let ((acc (coerce (aref (matrix-vals m) startr startc) ',accumulator-element-type)))
           (declare (type ,accumulator-element-type acc))
           (with-map-range m ,element-type startr endr startc endc (a i j)
             (when (> (aref a i j) acc)
               (setf acc (aref a i j))))
           acc)))))

(macrolet ((frob (type)
	     `(def-matrix-min-max ,type)))
  (frob double-float-matrix)
  (frob single-float-matrix)
  (frob ub8-matrix)
  (frob ub16-matrix)
  (frob ub32-matrix)
  (frob sb8-matrix)
  (frob sb16-matrix)
  (frob sb32-matrix)
  (frob fixnum-matrix)
  (frob bit-matrix)
  (frob integer-matrix)
  (frob real-matrix))
