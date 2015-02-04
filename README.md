# Racket mode for GNU Emacs

[![Build Status](https://travis-ci.org/greghendershott/racket-mode.png?branch=master)](https://travis-ci.org/greghendershott/racket-mode)
[![MELPA](http://melpa.org/packages/racket-mode-badge.svg)](http://melpa.org/#/racket-mode)

This provides a major mode to edit [Racket] source files, as well as a
major mode for a Racket REPL. The edit/run experience is similar to
[DrRacket].

[Racket]: http://www.racket-lang.org/
[DrRacket]: http://docs.racket-lang.org/drracket/

- Focus on Racket.
  - Mode line and menu say `Racket`.
  - Omit stuff for various current and historical Schemes that's not
    applicable to Racket.

- Use [DrRacket] concepts where applicable.
  - A simple and obvious way to "run" a file.
  - Allow interaction in the REPL, but the effect is wiped on the next
    run.
  - A simple way to run unit tests (to run the `test` submodule).

- More thorough syntax highlighting ("font-lock"):
  - All Racket keywords, built-ins, self-evals, and so on.
  - All variations of `define` for functions and variables.

- Correct indentation of Racket forms, including `for/fold` and
  `for*/fold`.

- Compatible with Emacs **24.3+**.

- [More](Reference.md).

## Caveats

- If you've used other Lisps and Schemes before, you might prefer
  [Geiser], which is very sophisticated.

- Although I dogfood this -- use it constantly to code Racket -- it is
  beta quality. My total experience writing Emacs modes consists of
  writing this mode.

- Pull requests from smarter/wiser people are welcome.

- Please report issues [here][issues].

[issues]: https://www.github.com/greghendershott/racket-mode/issues
[Geiser]: http://www.nongnu.org/geiser/

## Install

The recommended way to use `racket-mode` is to install the package
from [MELPA]. <kbd>M-x package-install</kbd>, `racket-mode`.

[MELPA]: http://melpa.org/

> **TIP**: To use MELPA add the following to your `~/.emacs` or
> `~/.emacs.d/init.el`:
>
> ```
> (require 'package)
> (add-to-list 'package-archives
>              '("melpa" . "http://melpa.org/packages/")
>              t)
> ```

## Configure

To start, there are only two [variables](Reference.md#variables) you
_might_ need to set:

- **racket-racket-program**, the name or pathname of the Racket
  executable. This defaults to `Racket.exe` on Windows else `racket`.

- Set **racket-raco-program**, the name or pathname of the Raco
  executable. This defaults to `Raco.exe` on Windows else `raco`.

On Windows or Linux, these defaults will probably work for you.

On OS X, downloading Racket doesn't add its `bin` directory to your
`PATH`. Even after you add it, GUI Emacs doesn't automatically use
your path (unless you use the handy [exec-path-from-shell] package).
Therefore you may want to set both of these to be full pathames.

[exec-path-from-shell]: http://melpa.org/#/exec-path-from-shell

You can `setq` these directly in your Emacs init file (`~/.emacs` or
`~/.emacs.d/init.el`), or, use <kbd>M-x Customize</kbd>, as you
prefer.

### Key bindings

To customize things like key bindings, you can use `racket-mode-hook`
in your Emacs init file. For example, although <kbd>F5</kbd> is bound
to the **racket-run** command, let's say you wanted <kbd>C-c r</kbd>
to be an additional binding:

```cl
(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c r") 'racket-run)))
```

## Documentation

Within Emacs, use the usual help functions.

- Type <kbd>C-h m</kbd> to get help about the modes in effect for the
  current buffer, including a list of key bindings and commands.

- To see help about a specific command, for example `racket-run`, type
  <kbd>C-h f</kbd> and then <kbd>racket-run</kbd>.

Here on GitHub you can browse the [Reference](Reference.md).

## Contributing

Pull requests are welcome! See [CONTRIBUTING.md](CONTRIBUTING.md).

## Background/Motivation

I started this project accidentally, while trying to figure out a
font-lock issue with [Quack] under Emacs 24.2.

[Quack]: http://www.neilvandyke.org/quack/

Knowing nothing about how to make a mode in Emacs, I tried to isolate
the problem by making a simple major mode, then adding things until it
broke. It didn't break and I ended up with this.

I took various `.emacs.d` hacks that I'd previously made to use with
Quack, and rolled them into this mode.

Also, I'd recently spent time adding Racket fontification to the
Pygments project, and wanted richer font-lock.

Also, I had experienced issues with `enter!` not always reloading
modules in recent versions of Racket, and came up with a DrRacket-like
alternative, `run!`.

Finally, I remembered that when I was new to Racket and Emacs, I got
confused by the _Scheme_ menu. It has options that work with various
Schemes over the years, but which are N/A for Racket. I would stare it
and wonder, "Um, how do I just 'run my program'??". I figured a fresh
focus on Racket might be helpful, especially for other folks
transitioning from using DrRacket.

Update, Jan 2014: After I had used this for a long time, putting up
with its quirks, someone put it on MELPA. That nudged me to take
another look, learn more about Elisp and Emacs modes, and improve
it. Although I still feel like an amateur, it has probably improved
from alpha to beta quality.

Please see the [Acknowledgments].

[Acknowledgments]: https://github.com/greghendershott/racket-mode/blob/master/THANKS.md
