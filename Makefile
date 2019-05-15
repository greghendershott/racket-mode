ifdef @$(EMACSBIN)
  EMACSBIN = @$(EMACSBIN)
else
  EMACSBIN = $(shell if [ -z "`which emacs`" ]; then echo "Emacs executable not found"; exit 1; else echo emacs; fi)
endif

BATCHEMACS = $(EMACSBIN) --batch --no-site-file -q -eval '(progn (add-to-list (quote load-path) "${PWD}/") (package-initialize))'

BYTECOMP = $(BATCHEMACS) -eval '(progn (require (quote bytecomp)) (setq byte-compile-warnings t) (setq byte-compile-error-on-warn t))' -f batch-byte-compile

.PHONY : help show-versions clean compile deps test test-elisp test-racket

help:
	@echo "Targets: show-versions, clean, compile, deps, test, test-elisp, test-racket"

show-versions:
	@echo `which racket`
	@racket --version
	@echo `which $(EMACSBIN)`
	@$(EMACSBIN) --version

%.elc : %.el
	$(BYTECOMP) $<


ELCS := $(patsubst %.el,%.elc,$(wildcard *.el))

clean:
	-rm $(ELCS) 2> /dev/null

compile: show-versions $(ELCS)

# Install packages we depend on. Intended for one-time use by
# developers and for Travis CI. (Normal users of the package get these
# deps automatically as a result of our Package-Requires in
# racket-mode.el)
deps:
	$(BATCHEMACS) -eval '(progn (add-to-list (quote package-archives) (cons "melpa" "http://melpa.org/packages/")) (package-initialize) (package-refresh-contents) (package-install (quote faceup)))'

test: test-racket test-elisp

test-racket:
	raco test ./racket/test/
	raco test -x ./racket/*.rkt
	raco test -x ./racket/commands/*.rkt

test-elisp:
	$(BATCHEMACS) -l ert -l racket-tests.el -f ert-run-tests-batch-and-exit
