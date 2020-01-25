EMACS ?= emacs
SANDBOX_DIR ?= ./sandbox

.PHONY: deps
deps:
	@mkdir -p "$(SANDBOX_DIR)"
	./with-gnu-utils/with-gnu-utils \
		./makem/makem.sh -vv -S "$(SANDBOX_DIR)" \
			--install-deps --install-linters

.PHONY: test
test:
	@echo "Using $(shell which $(EMACS))..."
	./with-gnu-utils/with-gnu-utils \
		./makem/makem.sh -vv -S "$(SANDBOX_DIR)" test

.PHONY: lint
lint:
	./with-gnu-utils/with-gnu-utils \
		./makem/makem.sh -vv -S "$(SANDBOX_DIR)" lint

.PHONY: clean
clean:
	rm -f -- *.elc **/*.elc *-autoloads.el **/*-autoloads.el *\~ **/*\~
