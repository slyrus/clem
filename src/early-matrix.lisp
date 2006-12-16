;;; -*- Mode: lisp; outline-regexp: ";;;;;*"; indent-tabs-mode: nil -*-;;;
;;;
;;; file: early-matrix.cl
;;; author: cyrus harmon
;;;

(in-package :clem)

(define-condition matrix-condition () ())

(define-condition matrix-error (simple-error matrix-condition) ())

(define-condition matrix-argument-error (matrix-error)
  ((cause :reader matrix-argument-error-cause
          :initarg :cause :initform ""))
  (:report (lambda (condition stream)
             (format stream "Error invalid arguments (~S)"
                     (matrix-argument-error-cause condition)))))

(deftype index-type ()
  '(integer 0 #.(1- array-dimension-limit)))

(declaim (inline matrix-vals))
(defclass matrix ()
  ((m :accessor matrix-vals)
   (rows :accessor matrix-rows :initarg :rows :initform 1 :type fixnum)
   (cols :accessor matrix-cols :initarg :cols :initform 1 :type fixnum)
   (initial-element :accessor initial-element :initarg :initial-element :initform 0d0)
   (adjustable :accessor adjustable :initarg :adjustable :initform nil)
   (resizeable :accessor resizable :initform nil))
  (:metaclass standard-matrix-class)
  (:element-type double-float)
  (:val-format "~4,9F")
  (:minval nil)
  (:maxval nil))

(defmethod make-load-form ((matrix matrix) &optional env)
  "Creates and returns a creation form and an initialization form
that can be used to externalize matrices."
  (make-load-form-saving-slots matrix :environment env))

(defgeneric allocate-matrix-vals (object &key rows cols adjustable initial-element))
(defmethod allocate-matrix-vals ((object matrix) &key rows cols adjustable (initial-element 0))
  (setf (slot-value object 'm)
	(make-array (list rows cols)
		    :adjustable adjustable
		    :initial-element (coerce initial-element (element-type (class-of object)))
		    :element-type (element-type (class-of object))
		    )))
  
(defmethod shared-initialize :after
    ((object matrix) slot-names &rest initargs &key rows cols adjustable initial-element)
  (declare (ignore slot-names initargs rows cols adjustable initial-element))
  (apply #'allocate-matrix-vals object
         (append
          (list :rows (slot-value object 'rows))
          (list :cols (slot-value object 'cols))
          (list :adjustable (slot-value object 'adjustable))
          (when (slot-value object 'initial-element)
            (list :initial-element (slot-value object 'initial-element))))))

