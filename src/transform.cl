;;;
;;; File: transform.cl
;;; Description: affine transformations for the clem matrix package
;;; Author: Cyrus Harmon
;;; Time-stamp: "2005-07-10 10:24:05 sly"
;;;

(in-package :clem)

(defun transform-coord (x y xfrm)
  (let ((coord1 (make-instance 'double-float-matrix :rows 3 :cols 1)))
    (setf (mref coord1 0 0) (coerce x 'double-float)
	  (mref coord1 1 0) (coerce y 'double-float)
	  (mref coord1 2 0) 1d0)
    (mat-mult xfrm coord1)))

;;; i need to rethink what to do about the output matrix for the
;;; moment I pass it in and it is the same size as the input matrix i
;;; should probably compute the required size of the thing and make a
;;; new matrix as apporpriate.
(defmethod transform-matrix (m n xfrm &key u v y x (background nil background-supplied-p))
  (let ((xfrm-shift (mat-copy xfrm)))
    (when (and u v)
      (let ((pre-shift1 (make-affine-transformation
			:y (car u) :x (car v)))
	    (pre-shift2 (make-affine-transformation
			 :y-scale (log (/ (- (cdr u) (car u)) (rows m)))
			 :x-scale (log (/ (- (cdr v) (car v)) (cols m))))))
;	(print (cons 'req-size (cons () (- (cdr v) (car v)))))
;	(print (cons 'in-size (cons (rows m) (cols m))))
	(setf xfrm-shift (mat-mult xfrm-shift (mat-mult pre-shift1 pre-shift2)))))
    (when (and x y)
      (let ((post-shift (make-affine-transformation
			 :y (- (car y)) :x (- (car x))))
	    (post-shift2 (make-affine-transformation
			  :y-scale (log (/ (rows n) (- (cdr y) (car y)))) 
			  :x-scale (log (/ (cols n) (- (cdr x) (car x)))))))
	(setf xfrm-shift (mat-mult post-shift (mat-mult post-shift2 xfrm-shift)))))
    (print-matrix xfrm-shift)

    (apply #'%transform-matrix m n xfrm-shift
	   (when background-supplied-p (list :background background)))))

;;; Creates 3x3 matrix that represents an affine transformation.
;;; since we have an arbitarty 2d matrix (row-major order) and we
;;; haven't really fixed x and y axes, we have some choice as
;;; to how to represent this. Current convention is that
;;; y == row and x == col, so rows 0 and 1 of this matrix are
;;; swapped WRT the usual parameterization of this kind of
;;; affine transformation matrix.
(defun make-affine-transformation (&key
				   (x 0d0)
				   (y 0d0)
				   (x-scale 0.0d0)
				   (y-scale 0.0d0)
				   (x-shear 0.0d0)
				   (y-shear 0.0d0)
				   (theta 0d0))
  (let ((xfrm (identity-matrix 3 :matrix-class 'double-float-matrix)))

    (setf (mref xfrm 0 0) (- (* (cos theta) (exp x-scale))
				   (* (sin theta) (exp y-scale) y-shear)))
    (setf (mref xfrm 0 1) (- (* (cos theta) (exp x-scale) x-shear)
				   (* (sin theta) (exp y-scale))))
    (setf (mref xfrm 0 2) (coerce y 'double-float))
    
    (setf (mref xfrm 1 0) (+ (* (sin theta) (exp x-scale))
				   (* (cos theta) (exp y-scale) y-shear)))
    (setf (mref xfrm 1 1) (+ (* (sin theta) (exp x-scale) x-shear)
				   (* (cos theta) (exp y-scale))))
    (setf (mref xfrm 1 2) (coerce x 'double-float))

    (setf (mref xfrm 2 0) 0d0)
    (setf (mref xfrm 2 1) 0d0)
    (setf (mref xfrm 2 2) 1d0)

    xfrm))
