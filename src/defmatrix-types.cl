
(in-package :clem)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype t-matrix ()
    :element-type t
    :accumulator-type t))
(defmatrixfuncs t-matrix
    :element-type t
    :accumulator-type t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype real-matrix (t-matrix)
    :element-type t
    :accumulator-type t))
(defmatrixfuncs real-matrix
  :element-type t
  :accumulator-type t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype float-matrix (real-matrix)
    :element-type t
    :accumulator-type t
    :val-format "~4,9F"))
(defmatrixfuncs float-matrix :element-type t :accumulator-type t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype integer-matrix (real-matrix)
    :element-type t
    :accumulator-type t
    :val-format "~d"
    :specialized-array t))
(defmatrixfuncs integer-matrix
  :element-type t
  :accumulator-type t
  :integral t
  :specialized-array t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype bit-matrix (integer-matrix) :element-type (unsigned-byte 1)
		 :accumulator-type (signed-byte 32)
		 :minval 0
		 :maxval 1
		 :val-format "~d"
		 :specialized-array t))
(defmatrixfuncs bit-matrix
    :element-type (unsigned-byte 1)
    :accumulator-type (signed-byte 32)
    :minval 0
    :maxval 1
    :integral t
    :specialized-array t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype signed-byte-matrix (integer-matrix) :element-type (signed-byte 8)
		 :accumulator-type (signed-byte 32)
		 :minval (- (expt 2 7))
		 :maxval (- (expt 2 7) 1)
		 :val-format "~d"
		 :specialized-array t))
(defmatrixfuncs signed-byte-matrix
    :element-type (signed-byte 8)
    :accumulator-type (signed-byte 32)
    :minval (- (expt 2 7))
    :maxval (- (expt 2 7) 1)
    :integral t
    :specialized-array t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype unsigned-byte-matrix (integer-matrix)
    :element-type (unsigned-byte 8)
    :accumulator-type (unsigned-byte 32)
    :minval 0
    :maxval (- (expt 2 8) 1)
    :val-format "~d"
    :specialized-array t))
(defmatrixfuncs unsigned-byte-matrix
    :element-type (unsigned-byte 8)
    :accumulator-type (unsigned-byte 32)
    :minval 0
    :maxval (- (expt 2 8) 1)
    :integral t
    :specialized-array t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype signed-word-matrix (integer-matrix)
    :element-type (signed-byte 16)
    :accumulator-type (signed-byte 32)
    :minval (- (expt 2 15))
    :maxval (- (expt 2 15) 1)
    :val-format "~d"
    :specialized-array t))
(defmatrixfuncs signed-word-matrix
    :element-type (signed-byte 16)
    :accumulator-type (signed-byte 32)
    :minval (- (expt 2 15))
    :maxval (- (expt 2 15) 1)
    :integral t
    :specialized-array t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype unsigned-word-matrix (integer-matrix)
    :element-type (unsigned-byte 16)
    :accumulator-type (unsigned-byte 32)
    :minval 0
    :maxval (- (expt 2 16) 1)
    :val-format "~d"
    :specialized-array t))
(defmatrixfuncs unsigned-word-matrix
    :element-type (unsigned-byte 16)
    :accumulator-type (unsigned-byte 32)
    :minval 0
    :maxval (- (expt 2 16) 1)
    :integral t
    :specialized-array t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype signed-long-matrix (integer-matrix)
    :element-type (signed-byte 32)
    :accumulator-type (signed-byte 32)
    :minval (- (expt 2 31))
    :maxval (- (expt 2 31) 1)
    :val-format "~d"
    :specialized-array t))
(defmatrixfuncs signed-long-matrix
    :element-type (signed-byte 32)
    :accumulator-type (signed-byte 32)
    :minval (- (expt 2 31))
    :maxval (- (expt 2 31) 1)
    :integral t
    :specialized-array t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype unsigned-long-matrix (integer-matrix)
    :element-type (unsigned-byte 32)
    :accumulator-type (unsigned-byte 32)
    :minval 0
    :maxval (- (expt 2 32) 1)
    :val-format "~d"
    :specialized-array t))
(defmatrixfuncs unsigned-long-matrix
    :element-type (unsigned-byte 32)
    :accumulator-type (unsigned-byte 32)
    :minval 0
    :maxval (- (expt 2 32) 1)
    :integral t
    :specialized-array t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype fixnum-matrix (integer-matrix) :element-type fixnum
		 :accumulator-type (unsigned-byte 32)
		 :minval most-negative-fixnum
		 :maxval most-positive-fixnum
		 :val-format "~d"
		 :specialized-array t))
(defmatrixfuncs fixnum-matrix
    :element-type fixnum
    :accumulator-type (unsigned-byte 32)
    :minval most-negative-fixnum
    :maxval most-positive-fixnum
    :integral t
    :specialized-array t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype single-float-matrix (float-matrix) :element-type single-float
		 :accumulator-type single-float
		 :initial-element 0f0
		 :minval most-negative-single-float
		 :maxval most-positive-single-float
		 :specialized-array t))
(defmatrixfuncs single-float-matrix :element-type single-float
		:accumulator-type single-float
		:minval most-negative-single-float
		:maxval most-positive-single-float
		:specialized-array t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmatrixtype double-float-matrix (float-matrix) :element-type double-float
		 :accumulator-type double-float
		 :initial-element 0d0
		 :minval most-negative-double-float
		 :maxval most-positive-double-float
		 :specialized-array t))
(defmatrixfuncs double-float-matrix :element-type double-float
		:accumulator-type double-float
		:minval most-negative-double-float
		:maxval most-positive-double-float
		:specialized-array t)

(defparameter *typed-matrix-types*
  '((double-float-matrix double-float "double-float") 
    (single-float-matrix single-float "single-float") 
    (fixnum-matrix fixnum "fixnum") 
    (signed-byte-matrix (signed-byte 8) "signed-byte") 
    (signed-word-matrix (signed-byte 16) "signed-word") 
    (signed-long-matrix (signed-byte 32) "signed-long") 
    (unsigned-byte-matrix (unsigned-byte 8) "unsigned-byte") 
    (unsigned-word-matrix (unsigned-byte 16) "unsigned-word") 
    (unsigned-long-matrix (unsigned-byte 32) "unsigned-long") 
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

;;; double-float-matrix add and subtr operations

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-move-element ,type-1 ,type-2)
		(def-matrix-move ,type-1 ,type-2)
		(def-matrix-add ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-add! ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-subtr ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-subtr! ,type-1 ,type-2 ,type-3 :suffix ,suffix))))

  (frob double-float-matrix double-float-matrix double-float-matrix)
  (frob double-float-matrix single-float-matrix double-float-matrix)
  (frob double-float-matrix unsigned-byte-matrix double-float-matrix)
  (frob double-float-matrix unsigned-word-matrix double-float-matrix)
  (frob double-float-matrix unsigned-long-matrix double-float-matrix)
  (frob double-float-matrix signed-byte-matrix double-float-matrix)
  (frob double-float-matrix signed-word-matrix double-float-matrix)
  (frob double-float-matrix signed-long-matrix double-float-matrix)
  (frob double-float-matrix bit-matrix double-float-matrix)
  (frob double-float-matrix fixnum-matrix double-float-matrix)

  (frob single-float-matrix single-float-matrix single-float-matrix)
  (frob single-float-matrix unsigned-byte-matrix single-float-matrix)
  (frob single-float-matrix unsigned-word-matrix single-float-matrix)
  (frob single-float-matrix unsigned-long-matrix single-float-matrix)
  (frob single-float-matrix signed-byte-matrix single-float-matrix)
  (frob single-float-matrix signed-word-matrix single-float-matrix)
  (frob single-float-matrix signed-long-matrix single-float-matrix)
  (frob single-float-matrix bit-matrix single-float-matrix)
  (frob single-float-matrix fixnum-matrix single-float-matrix)

  (frob unsigned-byte-matrix unsigned-byte-matrix unsigned-byte-matrix)
  (frob unsigned-word-matrix unsigned-word-matrix unsigned-word-matrix)
  (frob unsigned-long-matrix unsigned-long-matrix unsigned-long-matrix)

  (frob unsigned-byte-matrix bit-matrix unsigned-byte-matrix)
  (frob unsigned-word-matrix bit-matrix unsigned-word-matrix)
  (frob unsigned-long-matrix bit-matrix unsigned-long-matrix)

  (frob signed-byte-matrix bit-matrix signed-byte-matrix)
  (frob signed-word-matrix bit-matrix signed-word-matrix)
  (frob signed-long-matrix bit-matrix signed-long-matrix)
  
  (frob signed-long-matrix unsigned-byte-matrix signed-long-matrix)
  (frob signed-long-matrix unsigned-word-matrix signed-long-matrix))

(macrolet ((frob (type-1 type-2 type-3 &key suffix)
	     `(progn
		(def-matrix-add ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-matrix-subtr ,type-1 ,type-2 ,type-3 :suffix ,suffix)
		(def-move-element ,type-1 ,type-2)
		(def-matrix-move ,type-1 ,type-2))))
  (frob single-float-matrix double-float-matrix double-float-matrix)
  (frob double-float-matrix single-float-matrix double-float-matrix)
  (frob double-float-matrix unsigned-byte-matrix double-float-matrix)
  (frob double-float-matrix unsigned-word-matrix double-float-matrix)
  (frob double-float-matrix unsigned-long-matrix double-float-matrix)
  (frob double-float-matrix signed-byte-matrix double-float-matrix)
  (frob double-float-matrix signed-word-matrix double-float-matrix)
  (frob double-float-matrix signed-long-matrix double-float-matrix)
  (frob double-float-matrix bit-matrix double-float-matrix)
  (frob double-float-matrix fixnum-matrix double-float-matrix)

  (frob unsigned-byte-matrix double-float-matrix double-float-matrix)
  (frob unsigned-byte-matrix single-float-matrix single-float-matrix)

  (frob unsigned-word-matrix double-float-matrix double-float-matrix)
  (frob unsigned-word-matrix single-float-matrix single-float-matrix)

  (frob unsigned-long-matrix double-float-matrix double-float-matrix)
  (frob unsigned-long-matrix single-float-matrix single-float-matrix)

  (frob signed-byte-matrix double-float-matrix double-float-matrix)
  (frob signed-byte-matrix single-float-matrix single-float-matrix)

  (frob signed-word-matrix double-float-matrix double-float-matrix)
  (frob signed-word-matrix single-float-matrix single-float-matrix)

  (frob signed-long-matrix double-float-matrix double-float-matrix)
  (frob signed-long-matrix single-float-matrix single-float-matrix))




(macrolet ((frob (type-1 type-3 &key suffix)
	     `(progn
		(def-matrix-scale ,type-1 ,type-3 :suffix ,suffix)
		(def-matrix-scale! ,type-1 :suffix ,suffix))))
  (frob double-float-matrix double-float-matrix)
  (frob single-float-matrix single-float-matrix)
  (frob unsigned-byte-matrix unsigned-byte-matrix)
  (frob unsigned-word-matrix unsigned-word-matrix)
  (frob unsigned-long-matrix unsigned-long-matrix)
  (frob signed-byte-matrix signed-byte-matrix)
  (frob signed-word-matrix signed-word-matrix)
  (frob signed-long-matrix signed-long-matrix)
  (frob bit-matrix bit-matrix)
  (frob fixnum-matrix fixnum-matrix))

