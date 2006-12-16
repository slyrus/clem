
(defpackage #:clem-system (:use #:asdf #:cl))
(in-package #:clem-system)

;;;;
;;;; The following section customizes asdf to work with filenames
;;;; with a .cl extension and to put fasl files in a separate
;;;; directory.
;;;;
;;;; To enable this behvior, use asdf component type
;;;;  :clem-cl-source-file
;;;;
(defclass clem-cl-source-file (cl-source-file) ())

(defmethod source-file-type ((c clem-cl-source-file) (s module)) "cl")

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod asdf::output-files ((operation compile-op) (c clem-cl-source-file))
  (list (merge-pathnames *fasl-directory*
			 (compile-file-pathname (component-pathname c)))))

(defsystem :clem
  :name "clem"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :depends-on (ch-util)
  :components
  ((:static-file "version" :pathname #p"version.lisp-expr")
   (:module
    :src
    :components
    ((:cl-source-file "defpackage")
     (:cl-source-file "metaclasses" :depends-on ("defpackage"))
     (:cl-source-file "early-matrix" :depends-on ("defpackage" "metaclasses"))
     (:cl-source-file "matrix-classes" :depends-on ("defpackage" "metaclasses" "early-matrix"))
     (:cl-source-file "matrix" :depends-on ("matrix-classes"))
     (:cl-source-file "print" :depends-on ("matrix"))
     (:cl-source-file "typed-matrix" :depends-on ("defpackage" "matrix"))
     (:cl-source-file "mloop" :depends-on ("defpackage" "matrix"))
     (:cl-source-file "defmatrix" :depends-on ("typed-matrix"))
     (:cl-source-file "defmatrix-types" :depends-on ("defmatrix"))
     (:cl-source-file "scalar" :depends-on ("matrix"))
     (:cl-source-file "typed-matrix-utils" :depends-on ("typed-matrix"))
     (:cl-source-file "vector" :depends-on ("matrix"))
     (:cl-source-file "matrixops" :depends-on ("typed-matrix-utils"))
     (:cl-source-file "interpolation" :depends-on ("matrix" "defmatrix-types"))
     (:cl-source-file "transform" :depends-on ("matrix" "defmatrix-types" "interpolation"))
     (:module
      :typed-ops
      :components
      ((:cl-source-file "defmatrix-mref")
       (:cl-source-file "defmatrix-unary-op")
       (:cl-source-file "defmatrix-binary-op")
       (:cl-source-file "defmatrix-minmax")
       (:cl-source-file "defmatrix-move")
       (:cl-source-file "defmatrix-add")
       (:cl-source-file "defmatrix-equal")
       (:cl-source-file "defmatrix-subtr")
       (:cl-source-file "defmatrix-sum")
       (:cl-source-file "defmatrix-scale")
       (:cl-source-file "defmatrix-log")
       (:cl-source-file "defmatrix-abs")
       (:cl-source-file "defmatrix-hprod")
       (:cl-source-file "defmatrix-mult")
       (:cl-source-file "defmatrix-mult-block")
       (:cl-source-file "defmatrix-transform")
       (:cl-source-file "defmatrix-square")
       (:cl-source-file "defmatrix-subset-matrix")
       (:cl-source-file "defmatrix-convolve" :depends-on ("defmatrix-sum")))
      :depends-on ("defmatrix-types"
                   "matrix"
                   "mloop"
                   "scalar"
                   "interpolation"
                   "matrixops"
                   "typed-matrix-utils"))))
   (:static-file "bootstrap" :pathname #p"bootstrap.cl")
   (:static-file "COPYRIGHT")
   (:static-file "NEWS")
   (:static-file "ChangeLog")
   (:static-file "README")
   (:static-file "make-dist" :pathname #p"make-dist.sh")))

