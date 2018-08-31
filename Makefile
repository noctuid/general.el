EMACS ?= emacs

cask:
	EMACS=$(EMACS) cask --verbose --debug
	EMACS=$(EMACS) cask update --verbose --debug

test:
	@echo "Using $(shell which $(EMACS))..."
	cask exec buttercup -L .

compile:
	cask exec $(EMACS) -batch -Q --eval '(byte-compile-file "general.el")'

clean:
	rm -f *.elc

.PHONY: cask test clean
