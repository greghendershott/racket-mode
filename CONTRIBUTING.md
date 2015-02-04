# Package dependencies

racket-mode depends on a few packages:

- [`dash`](http://melpa.org/#/dash)
- [`faceup`](http://melpa.org/#/faceup)
- [`s`](http://melpa.org/#/s)

You can install these manually with <kbd>M-x package-install</kbd>,
or, run `make deps`. The latter is also used by `.travis.yml`.

# Reference.md

Although `Reference.md` is N/A for people using racket-mode within
Emacs, it is useful to have the features documented online, too. The
file is is generated from doc strings.

- If you add a brand-new command, `defcustom`, or `defface`, please
  also add it to appropriate list in `racket-make-doc.el`.

- Whenever you edit a doc string for a command, `defcustom`, or
  `defface`, please `make doc` and commit the updated `Reference.md`.

# Tests

Currently tests are on the light side. More are welcome.

Please do run `make test` to ensure your changes pass the existing
tests. Travis CI will also do this automatically on your pull request.

## Indentation

Indentation is tested by comparing to a couple reference files,
`example/*.rkt`.

If you change indentation, you may need to refresh each reference
file:

1. Open it.
2. Reindent it all
    1. <kbd>C-x h</kbd>
    2. <kbd>M-C-\\</kbd>
3. Save it.

## Font-lock

Font-lock is tested by comparing to a couple reference files,
`example/*.rkt.faceup`.

If you change font-lock, you may need to refresh each reference file:

1. Open it.
2. <kbd>M-x faceup-write-file</kbd>
