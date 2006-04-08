
(asdf:operate 'asdf:load-op :ch-asdf)
(asdf:operate 'asdf:load-op :ch-asdf-markup)

(defpackage #:clem-doc-system (:use #:asdf #:ch-asdf #:ch-asdf-markup #:cl))
(in-package #:clem-doc-system)

(defsystem :clem-doc
  :name "clem-doc"
  :author "Cyrus Harmon" 
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :depends-on (ch-asdf ch-bib ch-util clem com.gigamonkeys.markup)
  :components
  ((:static-file "make-tinaa-docs" :pathname #p"make-tinaa-docs.lisp")
   (:module
    :doc
    :components
    ((:module :gmarkup :pathname #P""
              :components ((:markup-file "clem")))
     (:module :latex :pathname #P""
              :depends-on (gmarkup)
              :components ((:markup-latex-file "clem")))
     (:module :pdf :pathname #P""
              :depends-on (gmarkup latex)
              :components ((:markup-pdf-file "clem")))
     (:module :xhtml :pathname #P""
              :depends-on (gmarkup)
              :components ((:markup-xhtml-file "clem")))
     (:css-file :simple)
     (:tinaa-directory :tinaa)))))

