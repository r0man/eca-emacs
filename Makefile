EMACS ?= emacs
EASK ?= eask

build:
	$(EASK) install-deps
	$(EASK) package
	$(EASK) install

ci: clean build compile checkdoc lint

compile:
	@echo "Compiling..."
	$(EASK) compile

checkdoc:
	$(EASK) lint checkdoc

lint:
	@echo "package linting..."
	$(EASK) lint package

clean:
	$(EASK) clean all

# Allow args to make commands
%:
	@:

.PHONY : ci compile checkdoc lint clean
