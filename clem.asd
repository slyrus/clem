
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
    ((:clem-cl-source-file "defpackage")
     (:clem-cl-source-file "metaclasses" :depends-on ("defpackage"))
     (:clem-cl-source-file "early-matrix" :depends-on ("defpackage" "metaclasses"))
     (:clem-cl-source-file "matrix-classes" :depends-on ("defpackage" "metaclasses" "early-matrix"))
     (:clem-cl-source-file "matrix" :depends-on ("defpackage" "metaclasses" "early-matrix" "matrix-classes"))
     (:clem-cl-source-file "typed-matrix" :depends-on ("defpackage" "matrix"))
     (:clem-cl-source-file "generics" :depends-on ("defpackage" "matrix"))
     (:clem-cl-source-file "mloop" :depends-on ("defpackage" "matrix"))
     (:clem-cl-source-file "defmatrix" :depends-on ("typed-matrix"))
     (:clem-cl-source-file "defmatrix-types" :depends-on ("defmatrix"))
     (:clem-cl-source-file "scalar" :depends-on ("matrix"))
     (:clem-cl-source-file "scalar-types" :depends-on ("defmatrix" "scalar"))
     (:clem-cl-source-file "typed-matrix-utils" :depends-on ("typed-matrix" "generics"))
     (:clem-cl-source-file "base-vector" :depends-on ("matrix"))
     (:clem-cl-source-file "row-vector" :depends-on ("matrix" "base-vector"))
     (:clem-cl-source-file "col-vector" :depends-on ("matrix" "base-vector"))
     (:clem-cl-source-file "matrixops" :depends-on ("typed-matrix-utils"))
     (:clem-cl-source-file "interpolation" :depends-on ("matrix" "defmatrix-types"))
     (:clem-cl-source-file "transform" :depends-on ("matrix" "defmatrix-types" "interpolation"))
     (:module
      :typed-ops
      :components
      ((:clem-cl-source-file "defmatrix-mref")
       (:clem-cl-source-file "defmatrix-unary-op")
       (:clem-cl-source-file "defmatrix-binary-op")
       (:clem-cl-source-file "defmatrix-minmax")
       (:clem-cl-source-file "defmatrix-move")
       (:clem-cl-source-file "defmatrix-add")
       (:clem-cl-source-file "defmatrix-subtr")
       (:clem-cl-source-file "defmatrix-sum")
       (:clem-cl-source-file "defmatrix-scale")
       (:clem-cl-source-file "defmatrix-log")
       (:clem-cl-source-file "defmatrix-abs")
       (:clem-cl-source-file "defmatrix-hprod")
       (:clem-cl-source-file "defmatrix-mult")
       (:clem-cl-source-file "defmatrix-mult-block")
       (:clem-cl-source-file "defmatrix-transform")
       (:clem-cl-source-file "defmatrix-square")
       (:clem-cl-source-file "defmatrix-subset-matrix")
       (:clem-cl-source-file "defmatrix-convolve" :depends-on ("defmatrix-sum")))
      :depends-on ("defmatrix-types"
                   "generics"
                   "mloop"
                   "scalar-types"
                   "interpolation"
                   "matrixops"
                   "typed-matrix-utils"))))
   (:static-file "bootstrap" :pathname #p"bootstrap.cl")
   (:static-file "COPYRIGHT")
   (:static-file "NEWS")
   (:static-file "README")
   (:static-file "make-dist" :pathname #p"make-dist.sh")))

