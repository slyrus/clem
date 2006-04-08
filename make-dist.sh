PACKAGE=clem
SYSTEMS=":${PACKAGE} :${PACKAGE}-test :${PACKAGE}-doc"

sbcl --noinform --noprint \
    --eval '(require :asdf)' \
    --eval "(pushnew (make-pathname :directory \""`pwd`"\") asdf:*central-registry*)" \
    --eval "(asdf:operate 'asdf:load-op 'ch-util)" \
    --eval "(load \"make-tinaa-docs.lisp\")" \
    --eval "(ch-util:make-dist ${SYSTEMS})" \
    --eval '(quit)'
