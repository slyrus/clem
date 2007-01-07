;;; mref.lisp
;;; macros, functions and methods for matrix element access
;;;
;;; Copyright (c) 2004-2006 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

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

(define-symbol-macro .mref-expanders. nil)

(defmacro with-typed-mref ((z element-type) &body body &environment env)
  (let ((vals (gensym "MATRIX-")))
    `(let ((,vals (matrix-vals ,z)))
       (declare (type (simple-array ,element-type *) ,vals))
       (symbol-macrolet
           ((.mref-expanders. ,(acons z vals (macroexpand-1 '.mref-expanders. env))))
         (macrolet
             ((mref (mat &rest args &environment env)
                (let ((vals (cdr (assoc mat (macroexpand-1 '.mref-expanders. env)))))
                  `(aref ,vals ,@args)))
              (row-major-mref (mat &rest args &environment env)
                (let ((vals (cdr (assoc mat (macroexpand-1 '.mref-expanders. env)))))
                  `(row-major-aref ,vals ,@args))))
           ,@body)))))

(defmacro matrix-total-size (matrix)
  `(array-total-size (matrix-vals ,matrix)))

(defmacro matrix-dimensions (matrix)
  `(array-dimensions (matrix-vals ,matrix)))

