;;;
;;; File: transform.cl
;;; Description: affine transformations for the clem matrix package
;;; Author: Cyrus Harmon
;;;

(in-package :clem)

;;; consider replacing these variables with either typed variables or
;;; an array accessing this is proving to be too slow...

(defclass affine-transformation (double-float-matrix)
  ((y-shift :accessor y-shift :initarg :y-shift :initform 0d0 :type 'double-float)
   (x-shift :accessor x-shift :initarg :x-shift :initform 0d0 :type 'double-float)
   (theta :accessor theta :initarg :theta :initform 0d0 :type 'double-float)
   (y-scale :accessor y-scale :initarg :y-scale :initform 0d0 :type 'double-float)
   (x-scale :accessor x-scale :initarg :x-scale :initform 0d0 :type 'double-float)
   (y-shear :accessor y-shear :initarg :y-shear :initform 0d0 :type 'double-float)
   (x-shear :accessor x-shear :initarg :x-shear :initform 0d0 :type 'double-float)
   (rows :accessor matrix-rows :initarg :rows :initform 3)
   (cols :accessor matrix-cols :initarg :cols :initform 3))
  (:metaclass standard-matrix-class)
  (:documentation "a matrix that represents an affine-transformation"))

(defgeneric transform-matrix
    (m n xfrm &key u v x y
       interpolation background
       update-transform)
  (:documentation
   "applies the affine transform xfrm to the contents of matrix m and
    places the contents in n"))

(defgeneric update-affine-transformation-matrix (xfrm))
(defgeneric copy-affine-transformation (xfrm))
(defgeneric move-affine-transformation (src dest))

(defun transform-coord (x y xfrm)
  "applies the affine transformation xfrm to the point {x,y} and
  returns the position of the point after applying the transformation"
  (let ((coord1 (make-instance 'double-float-matrix :rows 3 :cols 1)))
    (setf (mref coord1 0 0) (coerce x 'double-float)
	  (mref coord1 1 0) (coerce y 'double-float)
	  (mref coord1 2 0) 1d0)
    (let ((coord2 (mat-mult xfrm coord1)))
      (values (mref coord2 0 0) (mref coord2 1 0)))))

(defun compute-bounds (x1 y1 x2 y2 xfrm)
  "takes a region bound by x1 and x2 on the x-axis and y1 and y2 on
  the y-axis and returns the coordinates of the bounding rectangle
  after applying the affine transform xfrm"
  (multiple-value-bind (p1 q1)
      (transform-coord x1 y1 xfrm)
    (multiple-value-bind (p2 q2)
        (transform-coord x2 y2 xfrm)
      (multiple-value-bind (p3 q3)
          (transform-coord x1 y2 xfrm)
        (multiple-value-bind (p4 q4)
            (transform-coord x2 y1 xfrm)
          (values (min p1 p2 p3 p4) ;; x1'
                  (min q1 q2 q3 q4) ;; y1'
                  (max p1 p2 p3 p4) ;; x2'
                  (max q1 q2 q3 q4))))))) ;; y2'

;;; I need to rethink what to do about the output matrix for the
;;; moment I pass it in and it is the same size as the input matrix. I
;;; should probably compute the required size of the thing and make a
;;; new matrix as apporpriate.
;;;
;;; Ok, rethinking...
;;;
;;; we have an input matrix, an output matrix and an affine
;;; transformation, represented by a matrix.
;;; this is enough to go on but it would probably be nice to offer
;;; more parameters to make affine transforms easier to use.
;;;
;;; the problem is that both input and output matrices themselves can
;;; be considered as living in coordinate spaces. One approach would
;;; be to leave this as is, ranging from 0 to rows - 1 rows and 0 to
;;; cols - 1 cols. Alternatively, we can allow input and output
;;; coordinates, onto which the affine transform is applied and the
;;; appropriate transformed matrix generated. We probably also need to
;;; specify a pixel-space coordinate for the input and output matrices
;;; as well, although there are lots of possible to interpret
;;; those. Let'stick to the matrix-space coordinates and figure those
;;; at first:
;;;
;;; m = input matrix
;;; mr = input matrix rows - 1, mc = input matrix cols -1
;;;
;;; n = output matrix
;;; nr = output matrix rows - 1, nc = output matrix cols - 1
;;;
;;; xfrm - affine transformation, specifies the mapping of points from
;;; input space to output space
;;;
;;; u = (u1 . u2) begin and end x coordinates of input matrix
;;; v = (v1 . v2) begin and end y coordinates of input matrix
;;; 
;;; x = (x1 . x2) begin and end x coordinates of output matrix
;;; y = (y1 . y2) begin and end y coordinates of output matrix
;;; 
;;; examples:
;;;
;;; keeping the transformed matrix fully in the new matrix:
;;;  2x doubling transformation
;;;  u = (0 . 100), v = (0 . 100), x = (0 . 200), y = (0 . 200)
;;;
;;;  2x doubling of a matrix shifted
;;;  u = (100 . 200), v = (100 . 200), x = (200 . 400), y = (200 . 400)

(defmethod transform-matrix (m n xfrm
                             &key u v x y
                             (interpolation :nearest-neighbor interpolation-supplied-p)
                             (background nil background-supplied-p)
                             (update-transform t))
  (when update-transform
    (update-affine-transformation-matrix xfrm))
  (let ((xfrm-shift (mat-copy xfrm)))
    (unless u
      (setf u (cons 0 (cols m))))
    (unless v
      (setf v (cons 0 (rows m))))    
    (multiple-value-bind (x1 y1 x2 y2)
        (compute-bounds (car u) (car v) (cdr u) (cdr v) xfrm)
      (unless x
        (setf x (cons (floor x1) (ceiling x2))))
      (unless y
        (setf y (cons (floor y1) (ceiling y2)))))

    ;; Need to rework math to do the right thing here!

    (let ((pre-shift1 (make-affine-transformation
                       :y-shift (car v) :x-shift (car u)))
          (pre-shift2 (make-affine-transformation
                       :y-scale (log (/ (- (cdr v) (car v)) (rows m)))
                       :x-scale (log (/ (- (cdr u) (car u)) (cols m))))))
      (setf xfrm-shift (mat-mult xfrm-shift (mat-mult pre-shift1 pre-shift2))))
    (let ((post-shift (make-affine-transformation
                       :y-shift (- (car y)) :x-shift (- (car x))
                       ))
          (post-shift2 (make-affine-transformation
                        :y-scale (log (/ (rows n) (- (cdr y) (car y)))) 
                        :x-scale (log (/ (cols n) (- (cdr x) (car x))))
                        )))
      (setf xfrm-shift (mat-mult post-shift (mat-mult post-shift2 xfrm-shift))))
    (apply #'%transform-matrix m n xfrm-shift
           (append
            (when background-supplied-p (list :background background))
            (when interpolation-supplied-p (list :interpolation interpolation))))))


(defmethod update-affine-transformation-matrix ((xfrm affine-transformation))
  (setf (mref xfrm 0 0) (- (* (cos (theta xfrm)) (exp (x-scale xfrm)))
			   (* (sin (theta xfrm)) (exp (y-scale xfrm)) (y-shear xfrm))))
  (setf (mref xfrm 0 1) (- (* (cos (theta xfrm)) (exp (x-scale xfrm)) (x-shear xfrm))
			   (* (sin (theta xfrm)) (exp (y-scale xfrm)))))
  (setf (mref xfrm 0 2) (coerce (x-shift xfrm) 'double-float))
  
  (setf (mref xfrm 1 0) (+ (* (sin (theta xfrm)) (exp (x-scale xfrm)))
			   (* (cos (theta xfrm)) (exp (y-scale xfrm)) (y-shear xfrm))))
  (setf (mref xfrm 1 1) (+ (* (sin (theta xfrm)) (exp (x-scale xfrm)) (x-shear xfrm))
			   (* (cos (theta xfrm)) (exp (y-scale xfrm)))))
  (setf (mref xfrm 1 2) (coerce (y-shift xfrm) 'double-float))
  
  (setf (mref xfrm 2 0) 0d0)
  (setf (mref xfrm 2 1) 0d0)
  (setf (mref xfrm 2 2) 1d0)
  xfrm)

(defmethod copy-affine-transformation ((xfrm affine-transformation))
  (with-slots (y-shift x-shift theta y-scale x-scale y-shear x-shear) xfrm
    (make-instance 'affine-transformation
                 :y-shift y-shift
                 :x-shift x-shift
                 :theta theta
                 :y-scale y-scale
                 :x-scale x-scale
                 :y-shear y-shear
                 :x-shear x-shear)))

(defmethod move-affine-transformation ((src affine-transformation)
				       (dest affine-transformation))
  (with-slots (y-shift x-shift theta y-scale x-scale y-shear x-shear) src
    (setf (y-shift dest) y-shift)
    (setf (x-shift dest) x-shift)
    (setf (theta dest) theta)
    (setf (y-scale dest) y-scale)
    (setf (x-scale dest) x-scale)
    (setf (y-shear dest) y-shear)
    (setf (x-shear dest) x-shear)
    (update-affine-transformation-matrix dest)))

(defmethod decf-affine-transformation ((src affine-transformation)
				       (dest affine-transformation))
  (with-slots (y-shift x-shift theta y-scale x-scale y-shear x-shear) src
    (decf y-shift (y-shift dest))
    (decf x-shift (x-shift dest))
    (decf theta (theta dest))
    (decf y-scale (y-scale dest))
    (decf x-scale (x-scale dest))
    (decf y-shear (y-shear dest))
    (decf x-shear (x-shear dest))
    (update-affine-transformation-matrix src)))

(defmethod shared-initialize :after
    ((object affine-transformation) slot-names &rest args)
  (declare (ignore slot-names args))
  (update-affine-transformation-matrix object))

;;; Creates 3x3 matrix that represents an affine transformation.
;;; since we have an arbitarty 2d matrix (row-major order) and we
;;; haven't really fixed x and y axes, we have some choice as
;;; to how to represent this. Current convention is that
;;; y == row and x == col, so rows 0 and 1 of this matrix are
;;; swapped WRT the usual parameterization of this kind of
;;; affine transformation matrix.
(defun make-affine-transformation (&key
				   (x-shift 0d0)
				   (y-shift 0d0)
				   (x-scale 0.0d0)
				   (y-scale 0.0d0)
				   (x-shear 0.0d0)
				   (y-shear 0.0d0)
				   (theta 0d0))
  (make-instance 'affine-transformation
		 :x-shift x-shift :y-shift y-shift
		 :x-scale x-scale :y-scale y-scale
		 :x-shear x-shear :y-shear y-shear
		 :theta theta))

(defmethod parameters-list ((xfrm affine-transformation))
  (list 
   :y-shift (y-shift xfrm) 
   :x-shift (x-shift xfrm)
   :theta (theta xfrm)
   :y-scale (y-scale xfrm)
   :x-scale (x-scale xfrm)
   :y-shear (y-shear xfrm)
   :x-shear (x-shear xfrm)))
   
(defgeneric affine-transform (mat xfrm &key u v x y interpolation background matrix-class))
(defmethod affine-transform ((mat matrix)
                             (xfrm affine-transformation)
                             &key
                             u v x y
                             (interpolation nil interpolation-supplied-p)
                             (background nil background-supplied-p)
                             (matrix-class (class-of mat)))
  (unless u (setf u (cons 0 (cols mat))))
  (unless v (setf v (cons 0 (rows mat)))) 
  (multiple-value-bind (x1 y1 x2 y2)
      (compute-bounds (car u) (car v) (cdr u) (cdr v) xfrm)
    (unless x (setf x (cons (floor x1) (ceiling x2))))
    (unless y (setf y (cons (floor y1) (ceiling y2)))))
  (let ((rows (if y (truncate (- (cdr y) (car y)))
                  (rows mat)))
        (cols  (if x (truncate (- (cdr x) (car x)))
                   (cols mat))))
    (let ((m (make-instance matrix-class
                            :rows rows
                            :cols cols
                            :initial-element
                            (coerce 0 (element-type (class-of mat))))))
      (apply #'transform-matrix mat m xfrm
             (append
              (when u (list :u u))
              (when v (list :v v))
              (when x (list :x x))
              (when y (list :y y))
              (when background-supplied-p
                (list :background background))
              (when interpolation-supplied-p
                (list :interpolation interpolation))))
      m)))

(defun resize-matrix (m y x &key (interpolation :bilinear))
  (let ((oldy (rows m))
        (oldx (cols m)))
    (let ((xfrm (make-affine-transformation :x-scale (log (/ x oldx))
                                                  :y-scale (log (/ y oldy)))))
      (let ((n (affine-transform
                m xfrm
                :interpolation interpolation
                :u `(0 . ,oldx) :v `(0 . ,oldy)
                :x `(0 . ,x) :y `(0 . ,y))))
        n))))

