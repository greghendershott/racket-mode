EMACS ?= emacs

RACKET ?= racket

BATCHEMACS = $(EMACS) --batch --no-site-file -q -eval '(progn (add-to-list (quote load-path) "${PWD}/") (package-initialize))'

BYTECOMP = $(BATCHEMACS) -eval '(progn (require (quote bytecomp)) (setq byte-compile-warnings t) (setq byte-compile-error-on-warn t))' -f batch-byte-compile

.PHONY : help show-versions clean compile deps test test-elisp test-racket

help:
	@echo "Targets: show-versions, clean, compile, deps, test, test-elisp, test-racket"

show-versions:
	@echo `which $(RACKET)`
	@$(RACKET) --version
	@echo `which $(EMACS)`
	@$(EMACS) --version

%.elc : %.el
	$(BYTECOMP) $<

ELCS := $(patsubst %.el,%.elc,$(wildcard *.el))

clean:
	-rm $(ELCS) 2> /dev/null

compile: show-versions $(ELCS)

# Install packages we depend on for development and/or testing.
# Intended for one-time use by developers and for Travis CI. (Normal
# users get a subset of these deps automatically as a result of our
# Package-Requires in racket-mode.el)
deps:
	$(BATCHEMACS) -eval '(progn (add-to-list (quote package-archives) (cons "melpa" "http://melpa.org/packages/")) (package-initialize) (package-refresh-contents) (package-install (quote faceup)) (package-install (quote paredit)) (package-install (quote pos-tip)))'

test: test-racket test-elisp

test-racket:
	$(RACKET) -l raco test ./racket/test/
	$(RACKET) -l raco test -x ./racket/*.rkt
	$(RACKET) -l raco test -x ./racket/commands/*.rkt

test-elisp:
	$(BATCHEMACS) -l ert -l racket-tests.el -eval '(setq racket-program "$(RACKET)")' -f ert-run-tests-batch-and-exit
