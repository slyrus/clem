
(in-package :clem)

(defun make-intern (x &optional (package *package*))
  (intern (string-upcase x) package))

;;; this is taken from Peter Seibel's Practical Common Lisp
;;; book, p. 102
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))

