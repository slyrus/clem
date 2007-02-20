
(defpackage #:clem-benchmark-system (:use #:asdf #:cl))
(in-package #:clem-benchmark-system)

(defsystem :clem-benchmark
  :name "clem-benchmark"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :depends-on (ch-util clem)
  :components
  ((:module :benchmark
	    :components
	    ((:cl-source-file "defpackage")
	     (:cl-source-file "benchmarks" :depends-on ("defpackage"))))))

