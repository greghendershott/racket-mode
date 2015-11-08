# Reporting bugs

If you're going to report a bug -- thank you!

Please use <kbd>M-x racket-bug-report</kbd> to generate a buffer with
information that will help to reproduce and understand the bug:

- Emacs version
- racket-mode-version
- value of important racket-mode variables
- minor-modes that are active

Please copy this and paste in your bug report.

# Making pull requests

If you'd like to make a pull request -- thank you!

Here is some information to help you.

## Package dependencies

racket-mode depends on some other packages. In `racket-mode.el` see
the `Package-Requires:` line.

You can install these manually with <kbd>M-x package-install</kbd>,
or, run `make deps`. The latter is also used by `.travis.yml`.

The recent trend has been for racket-mode to depend on fewer packages,
not more. For example `dash.el` was dropped in favor of using native
Emacs Lisp constructs.

Having said that, if your PR truly needs a new package, please make
sure your PR updates all of:

1. the `Package-Requires:` line in `racket-mode.el`
2. the `deps` target in `makefile`

## Reference.md

Although `Reference.md` is N/A for people using racket-mode within
Emacs, it is useful to have the features documented online, too. The
file is is generated from doc strings.

- If you add a brand-new command, `defcustom`, or `defface`, please
  also add it to appropriate list in `racket-make-doc.el`.

- Whenever you edit a doc string for a command, `defcustom`, or
  `defface`, please `make doc` and commit the updated `Reference.md`.

## Tests

Currently tests are on the light side. More are welcome.

Please do run `make test` to ensure your changes pass the existing
tests. Travis CI will also do this automatically on your pull request.

### Indentation

Indentation is tested by comparing to a couple reference files,
`example/*.rkt`.

If you change indentation intentionally, you may need to refresh each
reference file:

1. Open it.
2. Reindent it all
    1. <kbd>C-x h</kbd>
    2. <kbd>M-C-\\</kbd>
3. Save it.

### Font-lock

Font-lock is tested by comparing to a couple reference files,
`example/*.rkt.faceup`.

If you change font-lock, you may need to refresh each reference file:

1. Open it.
2. <kbd>M-x faceup-write-file</kbd>
