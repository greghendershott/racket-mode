# Racket mode for GNU Emacs

[![Build Status](https://travis-ci.org/greghendershott/racket-mode.png?branch=master)](https://travis-ci.org/greghendershott/racket-mode)

This provides a major mode to edit [Racket] source files, as well as a
major mode for a Racket REPL. The edit/run experience is similar to
[DrRacket].

- Focus on Racket.
  - Mode line and menu say `Racket`.
  - Omit stuff for various current and historical Schemes that's N/A
    for Racket.

- Use [DrRacket] concepts where applicable.
  - A simple and obvious way to "run" a file: _Run_ <kbd>F5</kbd>.
  - Allow interaction in the REPL, but the effect is wiped on the next
    _Run_ (in contrast to using [`enter!`]).
  - A simple way to run unit tests (to run the `test` submodule).

- More thorough syntax highlighting ("font-lock"):
  - All Racket keywords, built-ins, self-evals, and so on.
  - All variations of `define` for functions and variables.

- Correct indentation of Racket forms, including `for/fold` and
  `for*/fold`.

- Compatible with Emacs **24.3+**.

## Caveats

- If you've used other Lisps and Schemes before, you may prefer
  [Geiser], which is very sophisticated.

- Although I dogfood this -- use it constantly to code Racket -- it is
  beta quality. My total experience writing Emacs modes consists of
  writing this mode.

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

- Set **Racket Program** to be the pathname of the `racket` executable.

- Set **Raco Program** to be the pathname of the `raco` executable.

That's the only required configuration.

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
  <kbd>F5</kbd> runs it. After which, you can use the `*Racket REPL*`
  buffer to inspect or experiment with the result. When you use
  <kbd>F5</kbd> again, `foo.rkt` is evaluated from scratch -- the
  custodian releases resources like threads and the evaluation
  environment is reset to the contents of `foo.rkt`. In other words,
  like [DrRacket], this provides the predictability of a "static"
  baseline, plus some interactive exploration.

- **racket-test** <kbd>C-F5</kbd> runs the `test` submodule
  (consisting of one or more `(module+ test ...)` forms in the current
  buffer).

    - **racket-fold-all-tests** <kbd>C-c C-f</kbd> uses `hideshow`
      mode to hide all `test` submodules. **racket-unfold-all-tests**
      <kbd>C-c C-u</kbd> shows them all again. This is handy if you
      like to interleave function definitions and `(module+ test ...)`
      tests, but sometimes want to "hide the clutter". In addition,
      see the **Hide/Show** menu for more-selective operations.

- Output in the `*Racket REPL*` buffer that describes a file and
  position is automatically "linkified". To visit, move point there
  and press <kdb>Return</kbd>, mouse click, or use a
  [Compilation mode command] such as <kbd>C-x \`</kbd> (next error).
  Examples of such text include:

    - Racket error messages.
    - `rackunit` test failure location messages.
    - `print`s of `#<path>` objects.

- **racket-visit-definition** <kbd>M-.</kbd> visits the definition of
  the symbol at point (or with a <kbd>C-u</kbd> prefix, <kbd>C-u
  M-.</kbd>, prompts for the symbol). Use <kbd>M-,</kbd> to return.
  
    > **NOTE**: This only finds symbols are defined in the current
    > namespace. (Use <kbd>F5</kbd> to **Run** the current buffer,
    > first.)

    > **NOTE**: If the definition is from Racket's `#%kernel` module,
    > it will tell you so but won't visit the definition site.

    > **NOTE**: Only visits the definition of module level identifiers
    > -- specifically, things for which Racket's
    > [`identifier-binding`] function returns a `list`, as opposed to
    > `'lexical`.

- **racket-visit-module** <kbd>C-M-.</kbd> visits the module at point
  inside a `require` form. Use <kbd>M-,</kbd> to return.

- **racket-doc** <kbd>C-c C-d</kbd> uses [`racket/help`] for the
  symbol at point (or with a prefix, <kbd>C-u C-c C-d</kbd> as
  prompted).

- **racket-describe** <kbd>C-c C-.</kbd> describes the function at
  point in a `*Racket Describe*` buffer. The intent is to give a quick
  reminder or introduction to a function, regardless of whether it has
  installed documentation -- and to do so within Emacs (without
  switching to a web browser window). This buffer is also displayed
  when you use company-mode and press <kbd>C-h</kbd> in the pop up
  completion list.

  - If the function has installed Racket documentation, then a
    simplified version of the HTML is presented in the buffer,
    including the "blue box", documentation prose, and examples.

  - Otherwise, the function's signature -- e.g. `(name arg-1-name
    arg-2-name)` is displayed. If the function has a Typed Racket
    type or a contract, then that is also displayed.

  You can quit the buffer by pressing <kbd>q</kbd>. Also, at the
  bottom of the buffer are Emacs buttons (which you may navigate among
  using <kbd>TAB</kbd> for visiting the definition or opening the full
  browser documentation (if any).

- **racket-cycle-paren-shapes** <kbd>C-c C-p</kbd> cycles the shape of
  the current s-expression among `()`, `[]`, and `{}`.

- **racket-tidy-requires** makes a single top-level `require` form,
  with modules sorted, one per line.

- **racket-trim-requires** does **racket-tidy-requires** and also
  deletes any unused modules.

- **racket-base-requires** does **racket-trim-requires**, and also
  changes a file that uses `#lang racket` to use `#lang racket/base`
  instead, adding explicit module requires as needed.

- **Completion**: racket-mode supports both Emacs 24.3+
  `completion-at-point` (<kbd>C-M-i</kbd>) and [`company-mode`].

    > **NOTE**: This only finds symbols in the current namespace. (Use
    > <kbd>F5</kbd> to **Run** the current buffer, first.)

- In the `*Racket REPL*` buffer you can issue some special
  commands. Some of them are the foundation for Emacs commands. Others
  are available only as a command in the REPL.

    - `,top`: Reset the REPL to "no file" (i.e. a base namespace).

    - `,run <file>`: Run the file. What **racket-run** <kbd>F5</kbd>
      uses. Either `"file.rkt"` is `file.rkt` OK.

    - `,doc <symbol-or-string>`: Look for `<symbol-or-string>` in
      Racket's documentation. What **racket-doc** <kbd>C-c C-d</kbd>
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
          not specified individually.

## Indentation

racket-mode includes indentation settings for many Racket built-in
functions and forms. To customize these further, you can use Elisp to
`(put <symbol> 'racket-indent-function <n>)`, where `<symbol>` is the
name of the Racket form (e.g. `'test-case`) and `n` is an integer. The
meaning of `n` is the same as for `lisp-indent-function` and
`scheme-indent-function`: Indent the first `n` arguments specially
(like ordinary function arguments), and then indent any further
arguments like a body.

For example in your `.emacs` file you could use:

```cl
(put 'test-case 'racket-indent-function 1)
```

to change the indent of `test-case` from this:

```racket
(test-case foo
           blah
           blah)
```

to this:

```racket
(test-case foo
  blah
  blah)
```

Also:

* racket-mode uses a special indentation for forms starting with "def"
  and "with".

* racket-mode first looks for a property on `racket-indent-function`;
  if none is found, it checks `scheme-indent-function`. That way you
  can use custom indents you may have already defined for use with
  scheme-mode.

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
with its quirks, someone put it on MELPA. That nudged me to take
another look, learn more about Elisp and Emacs modes, and improve
it. Although I still feel like an amateur, it has probably improved
from alpha to beta quality.

Please see the [Acknowledgments].

[Acknowledgments]: https://github.com/greghendershott/racket-mode/blob/master/THANKS.md
[Racket]: http://www.racket-lang.org/
[DrRacket]: http://docs.racket-lang.org/drracket/
[`enter!`]: http://docs.racket-lang.org/reference/interactive.html
[Geiser]: http://www.nongnu.org/geiser/
[Quack]: http://www.neilvandyke.org/quack/
[issues]: https://www.github.com/greghendershott/racket-mode/issues
[Compilation mode command]: http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html
[`racket/help`]: http://docs.racket-lang.org/reference/Interactive_Help.html
[`define-logger`]: http://docs.racket-lang.org/reference/logging.html#%28form._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._define-logger%29%29
[`current-directory`]: http://docs.racket-lang.org/reference/Filesystem.html#%28def._%28%28quote._~23~25kernel%29._current-directory%29%29
[MELPA]: http://melpa.milkbox.net/#/getting-started
[`company-mode`]: https://github.com/company-mode/company-mode
[`identifier-binding`]: http://docs.racket-lang.org/reference/stxcmp.html#%28def._%28%28quote._~23~25kernel%29._identifier-binding%29%29
