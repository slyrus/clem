
(defpackage #:clem-test-system (:use #:asdf #:cl))
(in-package #:clem-test-system)

(defsystem :clem-test
  :version "20040626.1"
  :depends-on (util clem)
  :components
  ((:module :test
	    :components
	    ((:file "defpackage")
	     (:file "test-clem")
	     (:file "test-clem2")
	     (:file "test-defmatrix")
	     (:file "bench-matrix")
	     )
	    :serial t
	    )))

(defparameter *this-system* :clem-test)

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
