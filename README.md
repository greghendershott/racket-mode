# Racket mode for GNU Emacs

This provides a major mode to edit [Racket] source files, as well as a
major mode for a Racket REPL. The edit/run experience is similar to
[DrRacket].

- Focus on Racket.
  - Mode line and menu say `Racket`.
  - Omit stuff for various current and historical Schemes that's N/A
    for Racket.

- Use DrRacket concepts where applicable.
  - A simple and obvious way to "run" a file: _Run_ <kbd>F5</kbd>.
  - Allow interaction in the REPL, but the effect is wiped on the next
    _Run_ (in contrast to using `enter!`).
  - A simple way to run unit tests (to run the `test` submodule).

- More thorough syntax highlighting ("font-lock"):
  - All Racket keywords, built-ins, self-evals, and so on.
  - All variations of `define` for functions and variables.

- Correct indentation of Racket forms, including `for/fold` and
  `for*/fold`.

- Compatible with Emacs **24.2+**. (With earlier versions of Emacs,
  font-lock will incorrectly highlight portions of things, e.g. the
  "min" in "aluminum" will be highlighted as the Racket keyword
  `min`.)

## Caveats

- If you've used other Lisps and Schemes before, you may prefer
  [Geiser], which is very sophisticated.

    > NOTE: I have an experimental
    > [`minor-mode` branch](https://github.com/greghendershott/racket-mode/tree/minor-mode)
    > that implements a Minor mode to add some features alongside
    > Geiser, as opposed to a stand-alone Major mode. But I'm not
    > using it day-to-day. My gripe is not so much with Geiser --
    > which is wonderful -- but with the Racket `enter!` evaluation
    > model that Geiser uses.

- Although I dogfood this -- use it constantly to code Racket -- it is
  alpha, or at best beta, quality. My total experience writing Emacs
  modes consists of writing this mode.

- Someone else proposed adding this to MELPA. Although I didn't
  object, and I've accepted pull requests to facilitate that, I wasn't
  seeking to promote it that way.

- Pull requests from smarter/wiser people are welcome.

- Please report issues [here][issues].

## Install

You can install the `racket-mode` package from [MELPA].

> Tip: To use MELPA in Emacs 24, add the following to your `.emacs` or
> `.emacs.d/init.el`:
>
> ```cl
> (require 'package)
> (add-to-list 'package-archives
>   '("melpa" . "http://melpa.milkbox.net/packages/") t)
> ```

## Configure

When a racket-mode window is active, type <kbd>M-x
customize-mode</kbd> (or choose **Customize** from the **Racket**
menu).

Set **Racket Program** to be the pathname of the `racket` executable.

That is the only required configuration.

If you wish, you can customize the other settings here. For example,
in addition to the usual font-lock faces, racket-mode defines a few
special ones:

- `racket-keyword-argument-face`: Used for Racket `#:keyword` arguments.

- `racket-selfeval-face`: Used for numbers, symbols, strings.

- `racket-paren-face`: Used for `( ) [ ] { }`. (I like to set these
  "dim", e.g. light gray, so the parens are less prominent.)

### Key bindings

To customize things like key bindings, you can use `racket-mode-hook`
in your `.emacs` or `.emacs.d/init.el`. For example, although
<kbd>F5</kbd> is bound to the **racket-run** command, let's say you
wanted <kbd>C-c r</kbd> to be an additional binding:

```cl
(add-hook 'racket-mode-hook
          '(lambda ()
             (define-key racket-mode-map (kbd "C-c r") 'racket-run)))
```

## Features

See the `Racket` menu. Most of the commands should be
self-explanatory. (If not, [report it][issues] and I'll improve the
documentation.)

A few notes:

- Assume you have `foo.rkt` as your current buffer. **racket-run**
  <kbd>F5</kbd> evaluates `foo.rkt`.  After which, you can use the
  `*Racket REPL*` buffer to inspect or experiment with the
  result. However when you use <kbd>F5</kbd> again, `foo.rkt` is
  evaluated from scratch -- the custodian releases resources like
  threads and the evaluation environment is reset to the contents of
  `foo.rkt`. In other words, like [DrRacket], this provides the
  predictability of a "static" baseline, plus some interactive
  exploration.

- **racket-test** <kbd>C-F5</kbd> runs the `test` submodule
  (consisting of one or more `(module+ test ...)` forms in the current
  buffer).

- Output in the `*Racket REPL*` buffer that describes a file and
  position is automatically "linkified". To visit the file at the
  position, click or use a [Compilation mode command] such as <kbd>C-x
  \`</kbd> (next error). Examples of such text include:

    - Racket error messages.
    - `rackunit` test failure location messages.
    - `print`s of `#<path>` objects.
    - output from the **racket-find-definition** command (see below).

- **racket-find-definition** <kbd>C-c C-d</kbd> tries to find the
  definition of the symbol at point (or with a prefix, <kbd>C-u C-c
  C-d</kbd>, as prompted). If found, it displays the file/location and
  function signature in the `*Racket REPL*` buffer. You can then use a
  [Compilation mode command] such as <kbd>C-x \`</kbd> to visit the
  definition.
  
    > **NOTE**: Racket doesn't provide anything like MIT Scheme's `pp`
    > or `pa` commands. This is something we hack ourselves -- see
    > defn.rkt -- and it's not perfect.
    >
    > This only finds symbols are defined in...
    >
    > 1. The current namespace. (Use <kbd>F5</kbd> to **Run** the
    > current buffer, first.)
    >
    > 2. Modules other than Racket `#%kernel`.

- **racket-help** <kbd>C-c C-h</kbd> uses [`racket/help`] for the symbol
  at point (or with a prefix, <kbd>C-u C-c C-h</kbd> as prompted).

- In the `*Racket REPL*` bufer you can issue some special
  commands. Some of them are the foundation for Emacs commands. Others
  are available only as a command in the REPL.

    - `,top`: Leave the curent file/module and go to the top level.

    - `,run <file>`: Run the file. What **racket-run** <kbd>F5</kbd>
      uses. Either `"file.rkt"` or `file.rkt` works.

    - `,def <symbol>`: Find definition of `<symbol>`. What
      **racket-find-definition** <kbd>C-c C-d</kbd> uses.

    - `,doc <symbol-or-string>`: Look for `<symbol-or-string>` in
      Racket's documentation. What **racket-help** <kbd>C-c C-h</kbd>
      uses.

    - `,cd`, `,pwd`: Change and show [`current-directory`].

    - `,log` controls the log output level, overall, as well as for
      specific named loggers created with [`define-logger`].

        - `,log`: Show the current levels.

        - `,log <logger> <level>`: Set a logger to show at least level
          `none`, `fatal`, `error`, `warning`, `info`, or `debug`.

        - `,log <logger> <level>`: Set a logger to use the default
          level.

        - `,log <level>`: Set the default level for all other loggers
          not specified indvidually.

## Background/Motivation

I started this project accidentally, while trying to figure out a
font-lock issue with [Quack] under Emacs 24.2.

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
with its quriks, someone put it on MELPA. That nudged me to take
another look, learn more about Elisp and Emacs modes, and improve
it. Although I still feel like an amateur, it has probably improved
from alpha to beta quality.

## Acknowledgments

- The existing Emacs Scheme mode and Inferior Scheme mode.

- The source code for Neil Van Dyke's Quack provided a model for many
  of the `scheme-indent-function` settings and smart paren closing.

[Racket]: http://www.racket-lang.org/
[DrRacket]: http://docs.racket-lang.org/drracket/
[Geiser]: http://www.nongnu.org/geiser/
[Quack]: http://www.neilvandyke.org/quack/
[issues]: https://www.github.com/greghendershott/racket-mode/issues
[Compilation mode command]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html
[`racket/help`]: http://docs.racket-lang.org/reference/Interactive_Help.html
[`define-logger`]: http://docs.racket-lang.org/reference/logging.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._define-logger%29%29
[`current-directory`]: http://docs.racket-lang.org/reference/Filesystem.html#%28def._%28%28quote._~23~25kernel%29._current-directory%29%29
[MELPA]: http://melpa.milkbox.net/#/getting-started
