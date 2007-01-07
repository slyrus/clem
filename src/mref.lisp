
(in-package :clem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mref and friends

(defmethod mref ((m matrix) &rest indices)
  (apply #'aref (matrix-vals m) indices))

(defmethod (setf mref) (v (m matrix) &rest indices)
  (setf (apply #'aref (matrix-vals m) indices) v))

(define-compiler-macro mref (matrix &rest indices)
  `(aref (matrix-vals ,matrix) ,@indices))

(define-compiler-macro (setf mref) (v matrix &rest indices)
  `(setf (aref (matrix-vals ,matrix) ,@indices) ,v))

(defmethod row-major-mref ((m matrix) index)
  (row-major-aref (matrix-vals m) index))

(define-compiler-macro row-major-mref (matrix &rest indices)
  `(row-major-aref (matrix-vals ,matrix) ,@indices))

(define-compiler-macro (setf row-major-mref) (v matrix &rest indices)
  `(setf (row-major-aref (matrix-vals ,matrix) ,@indices) ,v))

;;; with-typed-mref establishes variables for the matrix-vals of the
;;; matrix and local macros that shadow mref and 
;;; thanks to nyef, kpreid, rahul and Riastradh from #lisp for
;;; suggestions on how to make this work properly.
;;;
;;; (NOW FIXED) BIG NASTY HACK ALERT!
;;;
;;; Unfortunately, a naive implementation of with-typed-mref would
;;; rely on implementation-specific behavior of defmacro. The problem
;;; is that the environment might have dynamic extent and might not be
;;; around when we call the local mref macro, so this isn't guaranteed
;;; to work. It seems that the extent of the environment allows this
;;; to work on SBCL, but other implmenatations (or future versions of
;;; SBCL for that matter) aren't guaranteed to provide this, therefore
;;; this might break. Hopefully if it breaks it will do so in a really
;;; nasty, obvious way.
;;;
;;; it would be nice if this worked:
;;;
;; (defmacro with-typed-mref ((z element-type) &body body &environment env)
;;   (let ((vals (gensym "MATRIX-")))
;;     `(let ((,vals (matrix-vals ,z)))
;;        (declare (type (simple-array ,element-type *) ,vals))
;;        (macrolet ((mref (&whole whole mat &rest args)
;;                     (if (eql ',z mat)
;;                         `(aref ,',vals ,@args)
;;                         (macroexpand whole ,env)))
;;                   (row-major-mref (&whole whole mat &rest args)
;;                     (if (eql ',z mat)
;;                         `(row-major-aref ,',vals ,@args)
;;                         (macroexpand whole ,env))))
;;          ,@body))))
;;
;;; but we may not have access to the environment when we want to
;;; expand mref, so we need to do the following. Thanks to cmm on
;;; #lisp for the revised macro.
;;;

(defconstant +var-alist-sym+ (gensym))

(defmacro with-typed-mref ((z element-type) &body body &environment env)
  (let ((vals (gensym "MATRIX-")))
    `(let ((,vals (matrix-vals ,z)))
       (declare (type (simple-array ,element-type *) ,vals))
       , (multiple-value-bind (val-alist expanded-p)
             (macroexpand-1 +var-alist-sym+ env)
           (if expanded-p
               ;; no need or the MREF macrolet, it already exists
               `(symbol-macrolet ((,+var-alist-sym+
                                   ,(acons z vals val-alist)))
                  ,@body)
               ;; first contour
               `(symbol-macrolet ((,+var-alist-sym+ ,(acons z vals ())))
                  (macrolet ((mref (mat &rest args &environment env)
                               (let ((vals (cdr (assoc mat (macroexpand-1 ',+var-alist-sym+ env)))))
                                 `(aref ,vals ,@args)))
                             (row-major-mref (mat &rest args &environment env)
                               (let ((vals (cdr (assoc mat (macroexpand-1 ',+var-alist-sym+ env)))))
                                 `(row-major-aref ,vals ,@args))))
                    ,@body)))))))


(defmacro matrix-total-size (matrix)
  `(array-total-size (matrix-vals ,matrix)))

(defmacro matrix-dimensions (matrix)
  `(array-dimensions (matrix-vals ,matrix)))

