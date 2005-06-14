
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

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative
			      #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c clem-cl-source-file) (s module)) "cl")

(defmethod asdf::output-fasl-files ((operation compile-op) (c clem-cl-source-file)
				    (s module))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))

(defmethod asdf::output-files :around ((operation compile-op) (c clem-cl-source-file))
  (let ((m (compute-applicable-methods #'asdf::output-fasl-files (list operation c (component-system c)))))
    (if m
	(asdf::output-fasl-files operation c (component-system c))
	(call-next-method operation c))))

(defsystem :clem
  :name "clem"
  :author "Cyrus Harmon"
  :version "20050614.1"
  :components
  ((:module :src
	    :components
	    ((:clem-cl-source-file "defpackage")
	     (:clem-cl-source-file "metaclasses" :depends-on ("defpackage"))
	     (:clem-cl-source-file "matrix" :depends-on ("defpackage" "metaclasses"))
	     (:clem-cl-source-file "typed-matrix" :depends-on ("defpackage" "matrix"))
	     (:clem-cl-source-file "defmatrix" :depends-on ("defpackage" "matrix" "typed-matrix" "metaclasses"))
	     (:clem-cl-source-file "defmatrix-types" :depends-on ("defpackage" "matrix" "typed-matrix" "metaclasses" "defmatrix"))
	     (:clem-cl-source-file "defmatrix-move-add-subtr" :depends-on ("defmatrix-types"))
	     (:clem-cl-source-file "defmatrix-scale" :depends-on ("defmatrix-types"))
	     (:clem-cl-source-file "defmatrix-hprod" :depends-on ("defmatrix-types"))
	     (:clem-cl-source-file "typed-matrix-utils" :depends-on ("defpackage" "matrix" "typed-matrix"))
	     (:clem-cl-source-file "row-vector" :depends-on ("defpackage" "matrix"))
	     (:clem-cl-source-file "col-vector" :depends-on ("defpackage" "matrix"))
	     (:clem-cl-source-file "scalar" :depends-on ("defpackage" "matrix"))
	     (:clem-cl-source-file "matrixops" :depends-on ("defpackage" "matrix" "defmatrix-types" "typed-matrix-utils"))
	     ))))

