PACKAGE=clem
SYSTEMS=":${PACKAGE} :${PACKAGE}-test"

sbcl --noinform --noprint \
    --eval '(require :asdf)' \
    --eval "(pushnew (make-pathname :directory \""`pwd`"\") asdf:*central-registry*)" \
    --eval "(asdf:operate 'asdf:load-op 'ch-util)" \
    --eval "(ch-util:make-dist ${SYSTEMS})" \
    --eval '(quit)'
