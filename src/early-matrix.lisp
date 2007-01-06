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
   (dimensions :initarg :dimensions :initform '(1) :type (or list null))
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

(defgeneric allocate-matrix-vals (object &key adjustable initial-element &allow-other-keys))

(defmethod allocate-matrix-vals ((object matrix)
                                 &key
                                 (dimensions '(1))
                                 adjustable
                                 (initial-element 0))
  (setf (slot-value object 'm)
	(make-array dimensions
		    :adjustable adjustable
		    :initial-element (coerce initial-element (element-type (class-of object)))
		    :element-type (element-type (class-of object)))))
  
(defmethod shared-initialize :after
    ((object matrix) slot-names &rest initargs &key dimensions rows cols adjustable initial-element)
  (declare (ignore slot-names initargs dimensions adjustable initial-element)
           (optimize (debug 3)))
  (apply #'allocate-matrix-vals object
         (append
          (if (and rows cols)
              (list :dimensions (list rows cols))
              (list :dimensions (slot-value object 'dimensions)))
          (if (slot-boundp object 'adjustable)
              (list :adjustable (slot-value object 'adjustable)))
          (when (slot-value object 'initial-element)
            (list :initial-element (slot-value object 'initial-element))))))

(defmacro mref (matrix &rest indices)
  `(aref (matrix-vals ,matrix) ,@indices))

(defmacro row-major-mref (matrix &rest indices)
  `(row-major-aref (matrix-vals ,matrix) ,@indices))

(defmacro matrix-total-size (matrix)
  `(array-total-size (matrix-vals ,matrix)))

(defmacro matrix-dimensions (matrix)
  `(array-dimensions (matrix-vals ,matrix)))

(defmacro with-typed-mref ((m element-type) &body body &environment env)
  (let ((vals (gensym)))
    `(let ((,vals (matrix-vals ,m)))
       (declare (type (simple-array ,element-type *) ,vals))
       (macrolet ((mref (mat &rest args)
                    (if (eql ',m mat)
                        `(aref ,',vals ,@args)
                        `,(macroexpand `(mref ,',m ,@args) ,env))))
         ,@body))))

