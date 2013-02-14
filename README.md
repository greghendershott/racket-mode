# Racket mode for GNU Emacs

This is a major mode for editing Racket source files, as well as an
inferior mode for running Racket.

- Focus on Racket.
  - Mode line and menu say `Racket`.
  - Omit stuff for various current and historical Schemes that's N/A
    for Racket.

- Use DrRacket concepts where applicable.
  - A simple and obvious way to "Run" a `.rkt` file, fully restarting
    the REPL (in contrast to `enter!`).
  - A simple way to run unit tests (to run the `test` submodule).

- More thorough syntax highlighting ("font-lock"):
  - All Racket keywords, built-ins, self-evals, and so on.
  - All variations of `define` for functions and variables.

- Compatible with Emacs 23.4 and 24.2+.

## Caveats

This is alpha quality, i.e. version 0.1. My total experience writing
Emacs modes consists of writing this mode. (Pull requests from
smarter/wiser people are welcome.)

Please report issues to the [GitHub project page](https://www.github.com/greghendershott/racket-mode).

## Background/Motivation

I started this project accidentally, while trying to figure out a
font-lock issue with Quack under Emacs 24.2.

Knowing nothing about how to make a mode in Emacs, I tried to isolate
the problem by making a simple major mode, then adding things until it
broke. It didn't break and I ended up with this.

I took various `.emacs.d` hacks that I'd previously made to use with
Quack, and rolled them into this mode.

Also, I'd recently spent time adding Racket fontification to the
Pygments project.

I experienced issues with `enter!` not reloading modules in recent
versions of Racket, and came up with a DrRacket-like alternative,
`run!`.

Finally, I remembered that when I was new to Racket and Emacs, I got
confused by the _Scheme_ menu. It has options that work with various
Schemes over the years, but which are N/A for Racket. I figured a
fresh focus on Racket might be helpful.

Note: I think DrRacket is a _wonderful_ environment for developing
Racket. I started using Emacs when projects needed me to edit various
other file formats (like JavaScript, HTML, CSS, Markdown) in addition
to Racket.

## Acknowledgments

- The existing Emacs Scheme mode and Inferior Scheme mode.

- The source code for Neil Van Dyke's Quack provided a model for
  many of the scheme-indent-function settings, and smart paren closing.
