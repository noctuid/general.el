emacs ?= emacs

cask:
	$(shell EMACS=$(emacs) cask --verbose --debug)

test:
	@echo "Using $(shell which $(emacs))..."
	cask exec buttercup -L .

clean:
	rm -f *.elc

.PHONY: cask test clean
