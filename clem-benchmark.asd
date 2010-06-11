
(defpackage #:clem-benchmark-system (:use #:asdf #:cl))
(in-package #:clem-benchmark-system)

(defsystem :clem-benchmark
  :name "clem-benchmark"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.4.8"
  :depends-on (clem)
  :components
  ((:module :benchmark
	    :components
	    ((:cl-source-file "defpackage")
	     (:cl-source-file "benchmarks" :depends-on ("defpackage"))
             (:cl-source-file "bench-add" :depends-on ("benchmarks"))
             (:cl-source-file "bench-subtr" :depends-on ("benchmarks"))
             (:cl-source-file "bench-mult" :depends-on ("benchmarks"))
             (:cl-source-file "bench-hprod" :depends-on ("benchmarks"))
             (:cl-source-file "bench-scale" :depends-on ("benchmarks"))))))

