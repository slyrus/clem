
(require 'asdf)

(asdf:operate 'asdf:load-op 'clem)
(asdf:operate 'asdf:load-op 'tinaa)

(defun clem-system::make-tinaa-docs ()
  (asdf:operate 'asdf:load-op 'tinaa)
  (tinaa:document-system
   'package 'clem (asdf:component-pathname
                   (asdf:find-component
                    (asdf:find-system 'clem)
                    "tinaadoc"))))
  
(clem-system::make-tinaa-docs)
