
(in-package :clem)

(defmatrixfuncs t-matrix
    :element-type t
    :accumulator-type t)

(defmatrixfuncs number-matrix
    :element-type number
    :accumulator-type number)

(defmatrixfuncs real-matrix
    :element-type real
    :accumulator-type real)

(defmatrixfuncs complex-matrix
    :element-type complex
    :accumulator-type complex)

(defmatrixfuncs float-matrix
    :element-type float
    :accumulator-type float)

(defmatrixfuncs integer-matrix
    :element-type integer
    :accumulator-type integer)

(defmatrixfuncs bit-matrix
    :element-type (unsigned-byte 1)
    :accumulator-type (signed-byte 32)
    :minval 0
    :maxval 1)

(defmatrixfuncs sb8-matrix
    :element-type (signed-byte 8)
    :accumulator-type (signed-byte 32)
    :minval #.(- (expt 2 7))
    :maxval #.(- (expt 2 7) 1))

(defmatrixfuncs ub8-matrix
    :element-type (unsigned-byte 8)
    :accumulator-type (unsigned-byte 32)
    :minval 0
    :maxval #.(- (expt 2 8) 1))

(defmatrixfuncs sb16-matrix
    :element-type (signed-byte 16)
    :accumulator-type (signed-byte 32)
    :minval #.(- (expt 2 15))
    :maxval #.(- (expt 2 15) 1))

(defmatrixfuncs ub16-matrix
    :element-type (unsigned-byte 16)
    :accumulator-type (unsigned-byte 32)
    :minval 0
    :maxval #.(- (expt 2 16) 1))

(defmatrixfuncs sb32-matrix
    :element-type (signed-byte 32)
    :accumulator-type (signed-byte 32)
    :minval #.(- (expt 2 31))
    :maxval #.(- (expt 2 31) 1))

(defmatrixfuncs ub32-matrix
    :element-type (unsigned-byte 32)
    :accumulator-type (unsigned-byte 32)
    :minval 0
    :maxval #.(- (expt 2 32) 1))

(defmatrixfuncs fixnum-matrix
    :element-type fixnum
    :accumulator-type (unsigned-byte 32)
    :minval most-negative-fixnum
    :maxval most-positive-fixnum)

(defmatrixfuncs single-float-matrix
    :element-type single-float
    :accumulator-type single-float
    :minval most-negative-single-float
    :maxval most-positive-single-float)

(defmatrixfuncs double-float-matrix
    :element-type double-float
    :accumulator-type double-float
    :minval most-negative-double-float
    :maxval most-positive-double-float)

(defparameter *typed-matrix-types*
  '((double-float-matrix double-float "double-float") 
    (single-float-matrix single-float "single-float") 
    (fixnum-matrix fixnum "fixnum") 
    (sb8-matrix (signed-byte 8) "sb8") 
    (sb16-matrix (signed-byte 16) "sb16") 
    (sb32-matrix (signed-byte 32) "sb32") 
    (ub8-matrix (unsigned-byte 8) "ub8") 
    (ub16-matrix (unsigned-byte 16) "ub16") 
    (ub32-matrix (unsigned-byte 32) "ub32") 
    (bit-matrix (unsigned-byte 1) "bit")))

(defparameter *typed-matrix-types-hash* (make-hash-table :test 'equal))
(defparameter *typed-matrix-names-hash* (make-hash-table :test 'equal))

(mapc #'(lambda (l)
	  (destructuring-bind (matrix-type element-type type-name) l
	    ;;; (print (list matrix-type element-type type-name))
	    (setf (gethash element-type *typed-matrix-types-hash*) matrix-type)
	    (setf (gethash element-type *typed-matrix-names-hash*) type-name)))
      *typed-matrix-types*)

(defun get-matrix-type-for-type (type)
  (gethash type *typed-matrix-types-hash*))

(defun get-matrix-name-for-type (type)
  (gethash type *typed-matrix-names-hash*))

