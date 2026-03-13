EMACS ?= emacs
ORG_SLIPBOX_DIR ?= ../org-slipbox
PACKAGE = consult-org-slipbox.el
TEST_FILE = tests/test-consult-org-slipbox.el

.PHONY: compile
compile:
	$(EMACS) --batch -Q \
		-L . \
		-L $(ORG_SLIPBOX_DIR) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile \
		$(PACKAGE)

.PHONY: test
test:
	ORG_SLIPBOX_DIR="$(ORG_SLIPBOX_DIR)" \
	$(EMACS) --batch -Q \
		-L . \
		-L $(ORG_SLIPBOX_DIR) \
		--eval '(setq load-prefer-newer t)' \
		-l $(TEST_FILE) \
		-f ert-run-tests-batch-and-exit

.PHONY: checkdoc
checkdoc:
	$(EMACS) --batch -Q \
		-L . \
		-L $(ORG_SLIPBOX_DIR) \
		--eval "(progn (require 'checkdoc) (checkdoc-file \"$(PACKAGE)\"))"

.PHONY: check
check:
	$(MAKE) compile ORG_SLIPBOX_DIR="$(ORG_SLIPBOX_DIR)"
	$(MAKE) test ORG_SLIPBOX_DIR="$(ORG_SLIPBOX_DIR)"
	$(MAKE) checkdoc ORG_SLIPBOX_DIR="$(ORG_SLIPBOX_DIR)"
