
PACKAGE=clem
SYSTEMS=$(PACKAGE) $(PACKAGE)-test
CHUTILDIR=$(shell pwd)/../ch-util/
OPENPAREN=(
CLOSEPAREN=)

.PHONY: all dist $(PACKAGE) $(SYSTEMS)

all:
	@echo
	@echo "This system needs to be loaded into SBCL with asdf:load-op."
	@echo "Take a look at bootstrap.cl to get started"

FINDEXPRS=$(addprefix $(OPENPAREN)asdf:find-system :, $(addsuffix $(CLOSEPAREN), $(SYSTEMS)))

dist:
	sbcl --noinform --noprint \
	     --eval '(require :asdf)' \
	     --eval "(pushnew (make-pathname :directory \"$(shell pwd)\") asdf:*central-registry*)" \
	     --eval "(pushnew (make-pathname :directory (pathname-directory (truename \"$(CHUTILDIR)\"))) asdf:*central-registry*)" \
	     --eval "(asdf:operate 'asdf:load-op 'ch-util)" \
	     --eval "(asdf:operate 'asdf:load-op '$(PACKAGE))" \
	     --eval '(ch-util:make-dist $(FINDEXPRS))' \
	     --eval '(quit)'

