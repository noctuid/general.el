emacs ?= emacs

cask:
	$(shell EMACS=$(emacs) cask --verbose --debug)

test:
	@echo "Using $(shell which $(emacs))..."
	cask exec buttercup -L .

compile:
	cask exec emacs -Q --eval '(byte-compile-file "general.el")'

clean:
	rm -f *.elc

.PHONY: cask test clean
