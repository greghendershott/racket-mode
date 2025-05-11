.PHONY : help show-versions clean compile deps minimal-racket-deps test test-elisp test-racket test-racket-submod test-racket-plain test-racket-slow

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

test: test-racket test-elisp

######################################################################
# Emacs

batch-emacs := \
  $(EMACS) --batch -Q -L . \
  --eval '(require (quote package))' \
  --eval '(package-initialize)'

byte-compile := \
  $(batch-emacs) \
  -l bytecomp \
  --eval '(setq byte-compile-warnings (quote (not obsolete)))' \
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
      --eval '(unless (fboundp (quote lisp-data-mode)) (defalias (quote lisp-data-mode) (quote emacs-lisp-mode)))' \
      --eval '(package-refresh-contents)' \
      --eval '(package-install (quote compat))' \
      --eval '(package-install (quote faceup))' \
      --eval '(package-install (quote paredit))'

test-elisp:
	$(batch-emacs) \
      -l ert \
      -l test/racket-tests.el \
      --eval '(setq racket-program "$(RACKET)")' \
      -f ert-run-tests-batch-and-exit

######################################################################
# Racket

# This target is for a CI configuration using the Minimal Racket
# distribution. In that case we need some additional Racket packages
# from the main distribution -- as described in our end user docs at
# <https://www.racket-mode.com/#Minimal-Racket>.
minimal-racket-deps:
	$(RACKET) -l raco pkg install --auto \
      data-lib errortrace-lib macro-debugger-text-lib rackunit-lib \
      racket-index scribble-lib drracket-tool-text-lib

test-racket: test-racket-submod test-racket-plain

# Most tests exist inside `test` submodules of ordinary source files.
#
# Exclude racket/hash-lang.rkt because it fails to eval on older
# Rackets. Normally we only dynamic-require it. Furthermore its tests
# are in ./test/racket/hash-lang-test.rkt.
test-racket-submod:
	$(RACKET) -l raco test --submodule test --no-run-if-absent \
      $(filter-out ./racket/hash-lang.rkt, $(wildcard ./racket/*.rkt)) \
      $(wildcard ./racket/commands/*.rkt)

# Plus we do have some files in a special directory that consist of
# tests in the file's root module.
test-racket-plain:
	$(RACKET) -l raco test ./test/racket/

# Some very slow tests segregated in `slow-test` submodules so that
# they're not run by default.
test-racket-slow:
	$(RACKET) -l raco test --submodule slow-test ./racket/imports.rkt
	$(RACKET) -l raco test --submodule slow-test ./racket/commands/check-syntax.rkt
