# Racket mode for GNU Emacs

This is a major mode for editing [Racket] source files, as well as an
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

- Correct indentation of Racket forms, including `for/fold` and
  `for*/fold`.

- Compatible with Emacs **24.2+**. (With earlier versions of Emacs,
  font-lock will incorrectly highlight portions of things, e.g. the
  "min" in "aluminum" will be highlighted as the Racket keyword `min`.

## Caveats

This is alpha quality, i.e. version 0.1. My total experience writing
Emacs modes consists of writing this mode.

Pull requests from smarter/wiser people are welcome.

Please report issues [here][issues].

## Features

See the `Racket` menu. Most of the commands should be obvious. (If
not, [report it][issues] and I'll improve the documentation.)

One cluster of features is based on the idea of Emacs buttons in the
`*racket*` buffer, plus a command to activate the last such button:

- When `*racket*` buffer output includes text describing a file and
  location, the text is automatically "linkified" -- turned into an
  Emacs button. When clicked, the button opens the file at the
  position. Examples of such text include:
    - Racket error messages.
    - `rackunit` test failure location messages.
    - prints of `#<path>` objects.
    - output from the **racket-find-definition** command (see below).

- The command **racket-press-last-button** <kbd>C-c C-l</kbd> clicks
  the last button in the `*racket*` buffer, with the action taking
  place in the current window. For example if you type <kbd>F5</kbd>
  and there is an error, you can press <kbd>C-c C-l</kbd> to go to the
  location of the error.

- **racket-find-definition** <kbd>C-c C-d</kbd> tries to find the
  definition of the symbol at point (or with a prefix, <kbd>C-u C-c
  C-d</kbd>, as prompted). If found, it displays the file/location and
  function signature to the `*racket*` buffer. You can then use
  **racket-press-last-button** <kbd>C-c C-l</kbd> to go to the
  location of the definition.
  
  > **NOTE**: This can only find symbols that are defined in the
  > current namespace. So you may need to type <kbd>F5</kbd> to
  > **Run** the current buffer, before this will work.

- **racket-help** <kbd>C-c C-h</kbd> uses `racket/help` to find the
  symbol at point (or with a prefix, <kbd>C-u C-c C-h</kbd> as
  prompted). If found, a web browser opens.

- Errors are now displayed with context ("stack") information, and
  each file location in the "stack" is a navigable button (in case you
  want to jump to that location). The context is displayed in
  "reverse" order and the immediate error location is last, at the
  bottom. This is easier to parse visually, IMHO. Furthermore, it
  makes it work well with <kbd>C-c C-l</kbd>, because the immediate
  error will be the last button.


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

[Racket]: http://www.racket-lang.org/
[issues]: https://www.github.com/greghendershott/racket-mode/issues
