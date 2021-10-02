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

compile: $(elc-files)

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
      --eval '(package-install (quote paredit))' \
      --eval '(package-install (quote pos-tip))'

# October 2021: This is a hopefully temporary hack to work-around
# Windows Emacs being unable to validate Let's Encrypt certificates.
# This fetches the archives/packages even if the certificate fails.
# This is (probably) acceptable for CI and for Windows, only.
deps-win-ci:
	$(batch-emacs) \
      --eval '(add-to-list (quote package-archives) (cons "melpa" "$(melpa-url)"))' \
      --eval '(package-initialize)' \
      -l nsm \
      --eval '(setq network-security-level (quote low))' \
      --eval '(package-refresh-contents)' \
      --eval '(package-install (quote faceup))' \
      --eval '(package-install (quote paredit))' \
      --eval '(package-install (quote pos-tip))'

test: test-racket test-elisp

test-elisp:
	$(batch-emacs) \
      -l ert \
      -l racket-tests.el \
      --eval '(setq racket-program "$(RACKET)")' \
      -f ert-run-tests-batch-and-exit

test-racket:
	$(RACKET) -l raco test ./racket/test/
	$(RACKET) -l raco test -x ./racket/*.rkt
	$(RACKET) -l raco test -x ./racket/commands/*.rkt

test-slow:
	$(RACKET) -l raco test --submodule slow-test ./racket/imports.rkt
	$(RACKET) -l raco test --submodule slow-test ./racket/commands/check-syntax.rkt
