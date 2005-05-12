
(defpackage #:clem-system (:use #:asdf #:cl))
(in-package #:clem-system)

(defun add-registry-path (path)
  (pushnew (merge-pathnames
	    (make-pathname :directory (list :relative :up path))
 	    (make-pathname :directory (pathname-directory *load-truename*)))
	   asdf:*central-registry* :test 'equal))

(mapcar #'(lambda (x) (add-registry-path x))
	'("util"))

(defsystem :clem
    :name "clem"
    :author "Cyrus Harmon"
    :version "20050508.1"
    :depends-on (:util)
    :components
    ((:module :src
	      :components
	      ((:file "defpackage")
	       (:file "metaclasses" :depends-on ("defpackage"))
	       (:file "matrix" :depends-on ("defpackage" "metaclasses"))
	       (:file "typed-matrix" :depends-on ("defpackage" "matrix"))
	       (:file "defmatrix" :depends-on ("defpackage" "matrix" "typed-matrix" "metaclasses"))
	       (:file "defmatrix-types" :depends-on ("defpackage" "matrix" "typed-matrix" "metaclasses" "defmatrix"))
	       (:file "typed-matrix-utils" :depends-on ("defpackage" "matrix" "typed-matrix"))
	       (:file "row-vector" :depends-on ("defpackage" "matrix"))
	       (:file "col-vector" :depends-on ("defpackage" "matrix"))
	       (:file "scalar" :depends-on ("defpackage" "matrix"))
	       (:file "matrixops" :depends-on ("defpackage" "matrix" "defmatrix-types" "typed-matrix-utils"))
	       ))))

(defparameter *this-system* :clem)

(defparameter *fasl-directory*
  (make-pathname :directory '(:relative
			      #+sbcl "sbcl-fasl"
			      #+openmcl "openmcl-fasl"
			      #-(or sbcl openmcl) "fasl")))

(defmethod source-file-type ((c cl-source-file) (s (eql (find-system *this-system*))))
  "cl")

(defmethod asdf::output-fasl-files ((operation compile-op) (c cl-source-file)
				    (s (eql (find-system *this-system*))))
  (list (merge-pathnames *fasl-directory* (compile-file-pathname (component-pathname c)))))

