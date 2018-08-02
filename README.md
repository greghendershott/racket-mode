# Racket mode for GNU Emacs

[![Build Status](https://travis-ci.org/greghendershott/racket-mode.svg?branch=master)](https://travis-ci.org/greghendershott/racket-mode)
[![MELPA](https://melpa.org/packages/racket-mode-badge.svg)](https://melpa.org/#/racket-mode)

This provides a major mode to edit [Racket] source files, as well as a
major mode for a Racket REPL. The edit/run experience is similar to
[DrRacket].

[Racket]: https://www.racket-lang.org/
[DrRacket]: https://docs.racket-lang.org/drracket/

Compatible with **Emacs 24.3+** and **Racket 6.0+**.

## Install

The recommended way to use `racket-mode` is to install the package
from [MELPA]. <kbd>M-x</kbd> `package-install` <kbd>RET</kbd>
`racket-mode` <kbd>RET</kbd>.

[MELPA]: https://melpa.org/

> **TIP**: To use MELPA add the following to your `~/.emacs` or
> `~/.emacs.d/init.el`:
>
> ```
> (require 'package)
> (add-to-list 'package-archives
>              '("melpa" . "https://melpa.org/packages/")
>              t)
> ```

### Minimal Racket

If you have installed the minimal Racket distribution (for example by
using the [homebrew formula]): `racket-mode` needs some additional
packages (like `errortrace` and `macro-debugger`). A simple way to get
all these packages is to install the `drracket` package:

```shell
$ raco pkg install drracket
```

[homebrew formula]: https://github.com/Homebrew/homebrew-core/blob/master/Formula/minimal-racket.rb

## Update

Be aware that an Emacs package update doesn't necessarily fully update
Emacs' state. An example symptom is an "invalid function" error
message. You might need to restart Emacs. In some cases, you might
even need to:

1. Uninstall racket-mode
2. Exit and restart Emacs
3. Install racket-mode

If you still experience a problem, please `M-x racket-bug-report` and
submit an [issue].

## Configure

To start, there is only one [variable](Reference.md#variables) you
_might_ need to set:

- `racket-program` is the name or pathname of the Racket executable.
  It defaults to `Racket.exe` on Windows else `racket`.

On Windows or Linux, this default will probably work for you.

On macOS, downloading Racket doesn't add its `bin` directory to your
`PATH`. Even after you add it, GUI Emacs doesn't automatically use
your path (unless you use the handy [exec-path-from-shell] package).
Therefore you may want to set `racket-program` to a full pathname like
`/usr/racket/bin/racket`.

[exec-path-from-shell]: https://melpa.org/#/exec-path-from-shell

You can `setq` this directly in your Emacs init file (`~/.emacs` or
`~/.emacs.d/init.el`), or, use <kbd>M-x Customize</kbd>, as you
prefer.

### Start faster

You can use <kbd>M-x racket-mode-start-faster</kbd> to make the Racket
REPL start faster. [Read more](Reference.md#racket-mode-start-faster).


### Key bindings

To customize things like key bindings, you can use `racket-mode-hook`
in your Emacs init file. For example, although <kbd>F5</kbd> and
<kbd>C-c C-k</kbd> are bound to the `racket-run` command, let's say
you wanted <kbd>C-c r</kbd> to be an additional binding:

```cl
(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c r") 'racket-run)))
```

### Unicode input method

An optional Emacs input method, `racket-unicode`, lets you easily type
various Unicode symbols that might be useful when writing Racket code.

To automatically enable the `racket-unicode` input method in
`racket-mode` and `racket-repl-mode` buffers, put the following code
in your Emacs init file:

```cl
(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
```

For more information, see the documentation: <kbd>C-h f
racket-unicode-input-method-enable</kbd>.

### Completion

The usual `M-x complete-symbol` -- bound by default to
<kbd>C-M-i</kbd> -- works, drawing on all symbols in the current
Racket namespace.

> Tip: When you first visit a .rkt file, or edit it to change its
> `require`s, you may need to `racket-run` it to make the symbols
> available. Otherwise, you may get "stale" symbols, or just those
> from `racket/base`.

To have <kbd>TAB</kbd> do completion as well as indent, add the
following to your Emacs init file:

```cl
(setq tab-always-indent 'complete)
```

This changes the behavior of Emacs' standard `indent-for-tab-command`,
to which <kbd>TAB</kbd> is bound by default in the racket-mode edit
and REPL modes.

### Font-lock (syntax highlighting)

Font-lock (as Emacs calls syntax highlighting) can be controlled using
`font-lock-maximum-decoration`, which defaults to `t` (maximum). You
can set it to a number, where 0 is the lowest level. You can even
supply an association list to specify different values for different
major modes.

Historically you might choose a lower level for speed. These days you
might do so because you prefer a simpler appearance.

Racket-mode supports four, increasing levels of font-lock:

0: Just strings, comments, and `#lang`.

1: `#:keyword`s and self-evaluating literals like numbers, `'symbol`s,
   `'|symbols with spaces|`, regular expressions.

2: Identifiers in `define`-like and `let`-like forms.

3: Identifiers provided by `racket`, `typed/racket`, `racket/syntax`,
   and `syntax/parse`. (This level effectively treats Racket as a
   language, instead of a language for making languages.)

### paredit

You may want to add keybindings to `paredit-mode-map`:

- Bind <kbd>{</kbd> and <kbd>}</kbd> to `paredit-open-curly` and
  `paredit-close-curly`, respectively.

- Bind whatever keys you prefer for `paredit-wrap-square` and
  `paredit-wrap-curly`.

### smartparens

To use the default configuration that smartparens provides for Lisp
modes generally and for racket-mode specifically, add to your Emacs
init file:

```
(require 'smartparens-config)
```

### eldoc

By default racket-mode sets `eldoc-documentation-function` to `nil` --
no `eldoc-mode` support. You may set it to `racket-eldoc-function` in
a `racket-mode-hook` if you really want to use `eldoc-mode` with
Racket. But it is not a very satisfying experience because Racket is
not a very "eldoc-friendly" language. Although racket-mode attempts
to discover argument lists, contracts, or types this doesn't work in
many common cases:

- Many Racket primitives are defined in `#%kernel` or `#%runtime`.
  There's no easy way to determine their argument lists. Most are not
  `provide`d with a contract.

- Many of the interesting Racket forms are syntax (macros) not
  functions. There's no easy way to determine their "argument
  lists".

A more satisfying experience is to use `racket-describe` or
`racket-doc`.

## Documentation

Within Emacs, use the usual help functions.

- Type <kbd>C-h m</kbd> to get help about the modes in effect for the
  current buffer, including a list of key bindings and commands.

- To see help about a specific command, for example `racket-run`, type
  <kbd>C-h f</kbd> and then <kbd>racket-run</kbd>.

Here on GitHub you can browse the [Reference](Reference.md), which is
simply a markdown file generated from the same doc strings you see in
Emacs help.

## Contributing

Pull requests are welcome! See [CONTRIBUTING.md](CONTRIBUTING.md).

[Acknowledgments](https://github.com/greghendershott/racket-mode/blob/master/THANKS.md).

## Alternatives

- Emacs' built-in scheme-mode major mode plus the minor modes [Quack]
  and/or [Geiser].

[Quack]: https://www.neilvandyke.org/quack/
[Geiser]: https://www.nongnu.org/geiser/
