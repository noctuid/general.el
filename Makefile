emacs ?= emacs
CASK ?= cask
BEMACS = $(emacs) -batch -l elpa.el
LOAD = -l general.el

cask:
	$(shell EMACS=$(emacs) $(CASK) --verbose --debug)

test:
	@echo "Using $(shell which $(emacs))..."
	$(BEMACS) -l general-tests.el $(LOAD) -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc

.PHONY: cask test clean
