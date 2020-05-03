# Reporting bugs

If you're going to report a bug -- thank you!

Please use <kbd>M-x racket-bug-report</kbd> to generate a buffer with
information that will help to reproduce and understand the bug:

- Emacs version
- value of important Racket Mode variables
- minor-modes that are active

Please copy this and paste in your bug report.

# Making pull requests

If you'd like to make a pull request -- thank you!

Here is some information to help you.

## Package dependencies

Racket Mode depends on some other packages. In `racket-mode.el` see
the `Package-Requires:` line.

You can install these manually with <kbd>M-x package-install</kbd>,
or, run `make deps`. The latter is also used by `.travis.yml`.

The recent trend has been for Racket Mode to depend on fewer packages,
not more. For example `dash.el` was dropped in favor of using native
Emacs Lisp constructs. Likewise `s.el`.

Having said that, if your PR truly needs a new package, please make
sure your PR updates all of:

1. the `Package-Requires:` line in `racket-mode.el`
2. the `deps` target in `makefile`

## Pointing Emacs to your Git clone

After ensuring all dependencies of Racket Mode are installed, it
suffices to add the path to your local clone of Racket Mode to
`load-path` and require the package:

```elisp
(add-to-list 'load-path "/path/to/the/git-clone/dir")
(require 'racket-mode)
```

Note that these lines will override any previous references to Racket
Mode in your Emacs configuration.  In particular, if you have Racket
Mode installed as an Emacs package, after evaluating these lines you
will use Racket Mode from your local Git repository.

If you use `use-package`, you can simply replace

```elisp
(use-package racket-mode
    :ensure t)
```

with

```elisp
(use-package racket-mode
    :load-path "/path/to/the/git-clone/dir")
```

You might also need to:

* run <kbd>M-x package-delete</kbd> <kbd>racket-mode</kbd> so that the
  ELPA package is not loaded from `.emacs.d/elpa`,

* restart Emacs.

## doc/generate.el

We generate reference documentation from doc strings for commands, variables, and faces.

- If you add a brand-new command, `defcustom`, or `defface`, please
  also add it to appropriate list in `doc/generate.el`.

- Whenever you edit a doc string for a command, `defcustom`, or
  `defface`, please `cd doc && make clean && make` and commit the
  updated files.

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
