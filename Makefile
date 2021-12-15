.PHONY : help show-versions clean compile deps test test-elisp test-racket test-slow

help:
	@echo "Targets: show-versions, clean, compile, deps, test, test-elisp, test-racket, test-slow"

# Allow running with an emacs or racket executable other than the
# default on PATH. e.g. `EMACS=/path/to/emacs make`.
EMACS ?= emacs
RACKET ?= racket

show-versions:
	@echo `which $(RACKET)`
	@$(RACKET) --version
	@echo `which $(EMACS)`
	@$(EMACS) --version

batch-emacs := $(EMACS) --batch -Q -L . --eval '(package-initialize)'

byte-compile := \
  $(batch-emacs) \
  -l bytecomp \
  --eval '(setq byte-compile-warnings t)' \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile

%.elc : %.el
	$(byte-compile) $<

# Build an .elc file for every .el file in the top dir.
elc-files := $(patsubst %.el,%.elc,$(wildcard *.el))

clean:
	-rm $(elc-files) 2> /dev/null

compile: check-declares $(elc-files)

check-declares:
	$(batch-emacs) \
      -l check-declare \
      --eval '(unless (eq system-type (quote windows-nt)) (when (check-declare-directory default-directory) (kill-emacs 1)))'

# Install Emacs packages we depend on for development and/or testing.
# Intended to be run once per machine by developers, as well as by CI.
# (Normal users get a subset of these deps automatically as a result
# of our Package-Requires in racket-mode.el.)
melpa-url := https://melpa.org/packages/
deps:
	$(batch-emacs) \
      --eval '(add-to-list (quote package-archives) (cons "melpa" "$(melpa-url)"))' \
      --eval '(package-initialize)' \
      --eval '(package-refresh-contents)' \
      --eval '(package-install (quote faceup))' \
      --eval '(package-install (quote paredit))'

test: test-racket test-elisp

test-elisp:
	$(batch-emacs) \
      -l ert \
      -l test/racket-tests.el \
      --eval '(setq racket-program "$(RACKET)")' \
      -f ert-run-tests-batch-and-exit

# Files to test using `raco test -x`.
test-x-rkt-files := $(wildcard ./racket/*.rkt) $(wildcard ./racket/commands/*.rkt)
# Exclude hash-lang.rkt because it will fail to eval on older Rackets;
# normally we only dynamic-require it. Furthermore its tests are in
# ./test/racket/hash-lang-test.rkt.
test-x-rkt-files := $(filter-out ./racket/hash-lang.rkt, $(test-x-rkt-files))

test-racket:
	$(RACKET) -l raco test -x $(test-x-rkt-files)
	$(RACKET) -l raco test ./test/racket/

test-slow:
	$(RACKET) -l raco test --submodule slow-test ./racket/imports.rkt
	$(RACKET) -l raco test --submodule slow-test ./racket/commands/check-syntax.rkt
