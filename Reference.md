# Reference

- [Commands](#commands)
    - [Run](#run)
    - [Test](#test)
    - [Eval](#eval)
    - [Visit](#visit)
    - [Learn](#learn)
    - [Edit](#edit)
    - [Macro expand](#macro-expand)
    - [Other](#other)
- [Variables](#variables)
    - [General](#general)
    - [REPL](#repl)
    - [Other](#other)
    - [Experimental debugger](#experimental-debugger)
- [Faces](#faces)

# Commands

## Run

### racket-run
<kbd>C-c C-k</kbd> or <kbd>C-c C-c</kbd>

Save and evaluate the buffer in REPL.

With one C-u prefix, uses errortrace for improved stack traces.
Otherwise follows the [`racket-error-context`](#racket-error-context) setting.

With two C-u prefixes, instruments code for step debugging. See
[`racket-debug-mode`](#racket-debug-mode) and the variable [`racket-debuggable-files`](#racket-debuggable-files).

If point is within a Racket `module` form, the REPL "enters"
that submodule (uses its language info and namespace).

When you run again, the file is evaluated from scratch -- the
custodian releases resources like threads and the evaluation
environment is reset to the contents of the file. In other words,
like DrRacket, this provides the predictability of a "static"
baseline, plus the ability to explore interactively using the
REPL.

See also [`racket-run-and-switch-to-repl`](#racket-run-and-switch-to-repl), which is even more like
DrRacket's Run because it selects the REPL window (gives it the
focus), too.

When [`racket-retry-as-skeleton`](#racket-retry-as-skeleton) is true, if your source file has
an error, a "skeleton" of your file is evaluated to get
identifiers from module languages, `require`s, and definitions.
That way, things like completion and [`racket-describe`](#racket-describe) are more
likely to work while you edit the file to fix the error. If not
even the "skeleton" evaluation succeeds, you'll have only
identifiers provided by racket/base, until you fix the error and
run again.

Output in the `*Racket REPL*` buffer that describes a file and
position is automatically "linkified". Examples of such text
include:

- Racket error messages.
- `rackunit` test failure location messages.
- `print`s of `#<path>` objects.

To visit these locations, move point there and press RET or mouse
click. Or, use the standard `next-error` and `previous-error`
commands.

### racket-racket
<kbd>&lt;C-M-f5&gt;</kbd>

Do `racket <file>` in `*shell*` buffer.

### racket-profile
<kbd>C-c C-o</kbd>

Runs with profiling instrumentation and shows results.

Results are presented in a [`racket-profile-mode`](#racket-profile-mode) buffer, which
also lets you quickly view the source code.

You may evaluate expressions in the REPL. They are also profiled.
Use [`racket--profile-refresh`](#racket--profile-refresh) to see the updated results. (In
other words a possible workflow is: [`racket-profile`](#racket-profile) a .rkt file,
call one its functions in the REPL, and refresh the profile
results.)

Caveat: Only source files are instrumented. You may need to
delete compiled/*.zo files.

### racket-profile-mode
<kbd>M-x racket-profile-mode</kbd>

Major mode for results of [`racket-profile`](#racket-profile).

```
key             binding
---             -------

RET		racket--profile-visit
,		racket--profile-sort
g		racket--profile-refresh
n		racket--profile-next
p		racket--profile-prev
q		racket--profile-quit
z		racket--profile-show-zero


```


In addition to any hooks its parent mode `special-mode` might have run,
this mode runs the hook [`racket-profile-mode-hook`](#racket-profile-mode-hook), as the final step
during initialization.

### racket-logger
<kbd>C-c C-l</kbd>

Create the [`racket-logger-mode`](#racket-logger-mode) buffer and connect to logger output.

If the [`racket-repl-mode`](#racket-repl-mode) buffer is displayed in a window, split
that window and put the logger in the bottom window. Otherwise,
use `pop-to-buffer`.

### racket-logger-mode
<kbd>M-x racket-logger-mode</kbd>

Major mode for Racket logger output.

The customization variable [`racket-logger-config`](#racket-logger-config) determines the
levels for topics. During a session you may change topic levels
using [`racket-logger-topic-level`](#racket-logger-topic-level), bound to
"l".

For more information see:
  <https://docs.racket-lang.org/reference/logging.html>

```
key             binding
---             -------

C-c		Prefix Command
g		racket-logger-clear
l		racket-logger-topic-level
n		racket-logger-next-item
p		racket-logger-previous-item
w		toggle-truncate-lines
x		racket-logger-exit

C-c C-z		racket-repl


```


In addition to any hooks its parent mode `special-mode` might have run,
this mode runs the hook [`racket-logger-mode-hook`](#racket-logger-mode-hook), as the final step
during initialization.

### racket-debug-mode
<kbd>M-x racket-debug-mode</kbd>

Minor mode for debug breaks.

> This feature is **EXPERIMENTAL**!!! It is likely to have
> significant limitations and bugs. You are welcome to open an
> issue to provide feedback. Please understand that this feature
> might never be improved -- it might even be removed someday if
> it turns out to have too little value and/or too much cost.

How to debug:

1. "Instrument" code for step debugging. You can instrument
   entire files, and also individual functions.

   a. Entire Files

      Choose [`racket-run`](#racket-run) with two prefixes -- C-u C-u C-c C-c. The
      file will be instrumented for step debugging before it is run.
      Also instrumented are files determined by the variable
      [`racket-debuggable-files`](#racket-debuggable-files).

      The run will break at the first breakable position.

      Tip: After you run to completion and return to a normal
      REPL prompt, the code remains instrumented. You may enter
      expressions that evaluate instrumented code and it will
      break so you can step debug again.

   b. Function Definitions

      Put point in a function `define` form and C-u C-M-x to
      "instrument" the function for step debugging. Then in the
      REPL, enter an expression that causes the instrumented
      function to be run, directly or indirectly.

      You can instrument any number of functions.

      You can even instrument while stopped at a break. For
      example, to instrument a function you are about to call, so
      you can "step into" it:

        - M-. a.k.a. [`racket-visit-definition`](#racket-visit-definition).
        - C-u C-M-x to instrument the definition.
        - M-, a.k.a. [`racket-unvisit`](#racket-unvisit).
        - Continue stepping.

      Limitation: Instrumenting a function `require`d from
      another module won't redefine that function. Instead, it
      attempts to define an instrumented function of the same
      name, in the module the REPL is inside. The define will
      fail if it needs definitions visible only in that other
      module. In that case you'll probably need to use
      entire-file instrumentation as described above.

2. When a break occurs, the [`racket-repl-mode`](#racket-repl-mode) prompt changes. In
   this debug REPL, local variables are available for you to use
   and even to `set!`.

   Also, in the [`racket-mode`](#racket-mode) buffer where the break is located,
   [`racket-debug-mode`](#racket-debug-mode) is enabled. This minor mode makes the
   buffer read-only, provides visual feedback -- about the break
   position, local variable values, and result values -- and
   provides shortcut keys:

```
key             binding
---             -------

SPC		racket-debug-step
?		racket-debug-help
c		racket-debug-continue
h		racket-debug-run-to-here
n		racket-debug-next-breakable
o		racket-debug-step-over
p		racket-debug-prev-breakable
u		racket-debug-step-out


```

## Test

### racket-test
<kbd>&lt;C-f5&gt;</kbd> or <kbd>C-c C-t</kbd>

Run the `test` submodule.

With prefix, runs with coverage instrumentation and highlights
uncovered code.

Put your tests in a `test` submodule. For example:

    (module+ test
      (require rackunit)
      (check-true #t))

rackunit test failure messages show the location. You may use
`next-error` to jump to the location of each failing test.

See also:
- [`racket-fold-all-tests`](#racket-fold-all-tests)
- [`racket-unfold-all-tests`](#racket-unfold-all-tests)


### racket-raco-test
<kbd>M-x racket-raco-test</kbd>

Do `raco test -x <file>` in `*shell*` buffer.
To run <file>'s `test` submodule.

## Eval

### racket-send-region
<kbd>C-c C-r</kbd>

Send the current region (if any) to the Racket REPL.

### racket-send-definition
<kbd>C-M-x</kbd>

Send the current definition to the Racket REPL.

### racket-send-last-sexp
<kbd>C-x C-e</kbd>

Send the previous sexp to the Racket REPL.

When the previous sexp is a sexp comment the sexp itself is sent,
without the #; prefix.

## Visit

### racket-visit-definition
<kbd>M-.</kbd>

Visit definition of symbol at point.

Use M-x racket-unvisit to return.

Please keep in mind the following limitations:

- Only finds symbols defined in the current namespace. You may
  need to [`racket-run`](#racket-run) the current buffer, first.

- Only visits the definition of module-level identifiers --
  things for which Racket's `identifier-binding` function returns
  information. This does _not_ include things such as
  local (nested) function definitions or `racket/class` member
  functions. To find those in the same file, you'll need to use a
  normal Emacs text search function like `isearch-forward`.

- If the definition is found in Racket's `#%kernel` module, it
  will tell you so but won't visit the definition site.

### racket-visit-module
<kbd>C-M-.</kbd>

Visit definition of module at point, e.g. net/url or "file.rkt".

Use M-x racket-unvisit to return.

Note: Only works if you've [`racket-run`](#racket-run) the buffer so that its
namespace is active.

See also: [`racket-find-collection`](#racket-find-collection).

### racket-unvisit
<kbd>M-,</kbd>

Return from previous [`racket-visit-definition`](#racket-visit-definition) or [`racket-visit-module`](#racket-visit-module).

### racket-open-require-path
<kbd>C-c C-x C-f</kbd>

Like Dr Racket's Open Require Path.

Type (or delete) characters that are part of a module path name.
"Fuzzy" matches appear. For example try typing "t/t/r".

Choices are displayed in a vertical list. The current choice is
at the top, marked with "->".

- C-n and C-p move among the choices.
- RET on a directory adds its contents to the choices.
- RET on a file exits doing `find-file`.
- C-g aborts.

Note: This requires Racket 6.1.1.6 or newer. Otherwise it won't
error, it will just never return any matches.

### racket-find-collection
<kbd>M-x racket-find-collection</kbd>

Given a collection name, try to find its directory and files.

Takes a collection name from point (or, with a prefix, prompts you).

If only one directory is found, `ido-find-file-in-dir` lets you
pick a file there.

If more than one directory is found, `ido-completing-read` lets
you pick one, then `ido-find-file-in-dir` lets you pick a file
there.

Note: This requires the `raco-find-collection` package to be
installed. To install it, in `shell` enter:

    raco pkg install raco-find-collection

Tip: This works best with `ido-enable-flex-matching` set to t.
Also handy is the `flx-ido` package from MELPA.

See also: [`racket-visit-module`](#racket-visit-module) and [`racket-open-require-path`](#racket-open-require-path).

## Learn

### racket-describe
<kbd>C-c C-.</kbd>

Describe the identifier at point in a `*Racket Describe*` buffer.

The intent is to give a quick reminder or introduction to
something, regardless of whether it has installed documentation
-- and to do so within Emacs, without switching to a web browser.

This buffer is also displayed when you use `company-mode` and
press F1 or C-h in its pop up completion list.

- If the identifier has installed Racket documentation, then a
  simplified version of the HTML is presented in the buffer,
  including the "blue box", documentation prose, and examples.

- Otherwise, if the identifier is a function, then its signature
  is displayed, for example `(name arg-1-name arg-2-name)`. If it
  has a contract or a Typed Racket type, that is also displayed.

You can quit the buffer by pressing q. Also, at the bottom of the
buffer are Emacs buttons -- which you may navigate among using
TAB, and activate using RET -- for [`racket-visit-definition`](#racket-visit-definition) and
[`racket-doc`](#racket-doc).

### racket-doc
<kbd>C-c C-d</kbd>

View documentation of the identifier or string at point.

Uses the default external web browser.

If point is an identifier required in the current namespace that
has help, opens the web browser directly at that help
topic. (i.e. Uses the identifier variant of racket/help.)

Otherwise, opens the 'search for a term' page, where you can
choose among multiple possibilities. (i.e. Uses the string
variant of racket/help.)

With a C-u prefix, prompts for the identifier or quoted string,
instead of looking at point.

## Edit

### racket-fold-all-tests
<kbd>C-c C-f</kbd>

Fold (hide) all test submodules.

### racket-unfold-all-tests
<kbd>C-c C-u</kbd>

Unfold (show) all test submodules.

### racket-tidy-requires
<kbd>M-x racket-tidy-requires</kbd>

Make a single top-level `require`, modules sorted, one per line.

All top-level `require` forms are combined into a single form.
Within that form:

- A single subform is used for each phase level, sorted in this
  order: for-syntax, for-template, for-label, for-meta, and
  plain (phase 0).

  - Within each level subform, the modules are sorted:

    - Collection path modules -- sorted alphabetically.

    - Subforms such as `only-in`.

    - Quoted relative requires -- sorted alphabetically.

At most one module is listed per line.

Note: This only works for requires at the top level of a source
file using `#lang`. It does *not* work for `require`s inside
`module` forms.

See also: [`racket-trim-requires`](#racket-trim-requires) and [`racket-base-requires`](#racket-base-requires).

### racket-trim-requires
<kbd>M-x racket-trim-requires</kbd>

Like [`racket-tidy-requires`](#racket-tidy-requires) but also deletes unnecessary requires.

Note: This only works when the source file can be evaluated with
no errors.

Note: This only works for requires at the top level of a source
file using `#lang`. It does *not* work for `require`s inside
`module` forms. Furthermore, it is not smart about `module+` or
`module*` forms -- it may delete top level requires that are
actually needed by such submodules.

See also: [`racket-base-requires`](#racket-base-requires).

### racket-base-requires
<kbd>M-x racket-base-requires</kbd>

Change from `#lang racket` to `#lang racket/base`.

Adds explicit requires for modules that are provided by `racket`
but not by `racket/base`.

This is a recommended optimization for Racket applications.
Avoiding loading all of `racket` can reduce load time and memory
footprint.

Also, as does [`racket-trim-requires`](#racket-trim-requires), this removes unneeded
modules and tidies everything into a single, sorted require form.

Note: This only works when the source file can be evaluated with
no errors.

Note: This only works for requires at the top level of a source
file using `#lang`. It does *not* work for `require`s inside
`module` forms. Furthermore, it is not smart about `module+` or
`module*` forms -- it may delete top level requires that are
actually needed by such submodules.

Note: Currently this only helps change `#lang racket` to
`#lang racket/base`. It does *not* help with other similar conversions,
such as changing `#lang typed/racket` to `#lang typed/racket/base`.

### racket-indent-line
<kbd>M-x racket-indent-line</kbd>

Indent current line as Racket code.

This behaves like `lisp-indent-line`, except that whole-line
comments are treated the same regardless of whether they start
with single or double semicolons.

- Automatically indents forms that start with `begin` in the usual
  way that `begin` is indented.

- Automatically indents forms that start with `def` or `with-` in the
  usual way that `define` is indented.

- Has rules for many specific standard Racket forms.

To extend, use your Emacs init file to

    (put SYMBOL 'racket-indent-function INDENT)

where `SYMBOL` is the name of the Racket form (e.g. `'test-case`)
and `INDENT` is an integer or the symbol `'defun`. When `INDENT`
is an integer, the meaning is the same as for
`lisp-indent-function` and `scheme-indent-function`: Indent the
first `n` arguments specially and then indent any further
arguments like a body.

For example in your `.emacs` file you could use:

    (put 'test-case 'racket-indent-function 1)

to change the indent of `test-case` from this:

    (test-case foo
               blah
               blah)

to this:

    (test-case foo
      blah
      blah)

If `racket-indent-function` has no property for a symbol,
`scheme-indent-function` is also considered (although the with-x
indents defined by `scheme-mode` are ignored). This is only to
help people who may have extensive `scheme-indent-function`
settings, particularly in the form of file or dir local
variables. Otherwise prefer `racket-indent-function`.

### racket-smart-open-bracket
<kbd>[</kbd>

Automatically insert a `(` or a `[` as appropriate.

When [`racket-smart-open-bracket-enable`](#racket-smart-open-bracket-enable) is nil, this simply
inserts `[`. Otherwise, this behaves like the "Automatically
adjust opening square brackets" feature in Dr. Racket:

By default, inserts a `(`. Inserts a `[` in the following cases:

  - `let`-like bindings -- forms with `let` in the name as well
    as things like `parameterize`, `with-handlers`, and
    `with-syntax`.

  - `case`, `cond`, `match`, `syntax-case`, `syntax-parse`, and
    `syntax-rules` clauses.

  - `for`-like bindings and `for/fold` accumulators.

  - `class` declaration syntax, such as `init` and `inherit`.

When the previous s-expression in a sequence is a compound
expression, uses the same kind of delimiter.

To force insert `[`, use `quoted-insert`: C-q [.

Combined with [`racket-insert-closing`](#racket-insert-closing) this means that
you can press the unshifted `[` and `]` keys to get whatever
delimiters follow the Racket conventions for these forms. (When
`electric-pair-mode` or `paredit-mode` is active, you need not
even press `]`.

### racket-cycle-paren-shapes
<kbd>C-c C-p</kbd>

Cycle the sexpr among () [] {}.

### racket-backward-up-list
<kbd>C-M-u</kbd>

Like `backward-up-list` but works when point is in a string or comment.

Typically you should not use this command in Emacs Lisp --
especially not repeatedly. Instead, initially use
[`racket--escape-string-or-comment`](#racket--escape-string-or-comment) to move to the start of a
string or comment, if any, then use normal `backward-up-list`
repeatedly.

### racket-check-syntax-mode
<kbd>M-x racket-check-syntax-mode</kbd>

Analyze the buffer and annotate with information.

The buffer becomes read-only until you exit this minor mode.
However you may navigate the usual ways. When point is on a
definition or use, related items are highlighted and
information is displayed in the echo area. You may also use
special commands to navigate among the definition and its uses.

```
key             binding
---             -------

TAB		racket-check-syntax-mode-goto-next-def
.		racket-check-syntax-mode-goto-def
h		racket-check-syntax-mode-help
j		racket-check-syntax-mode-goto-next-def
k		racket-check-syntax-mode-goto-prev-def
n		racket-check-syntax-mode-goto-next-use
p		racket-check-syntax-mode-goto-prev-use
q		racket-check-syntax-mode-quit
r		racket-check-syntax-mode-rename
<backtab>	racket-check-syntax-mode-goto-prev-def


```


### racket-unicode-input-method-enable
<kbd>M-x racket-unicode-input-method-enable</kbd>

Set input method to `racket-unicode`.

The `racket-unicode` input method lets you easily type various
Unicode symbols that might be useful when writing Racket
code.

To automatically enable the `racket-unicode` input method in
`racket-mode` buffers use `M-x customize-variable <RET>
racket-mode-hook` or put the following code in your Emacs init
file:

    (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)

Likewise for `racket-repl-mode` buffers:

    (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)

To temporarily enable this input method for a single buffer you
can use `M-x racket-unicode-input-method-enable`.

Use `C-\` to toggle the input method.

When the `racket-unicode` input method is active, you can for
example type `All` and it is immediately replaced with `∀`. A few
other examples:

    omega     ω
    x_1       x₁
    x^1       x¹
    |A|       𝔸
    test-->>E test-->>∃ (racket/redex)

To see a table of all key sequences use `M-x
describe-input-method <RET> racket-unicode`.

If you don't like the highlighting of partially matching tokens you
can turn it off by setting `input-method-highlight-flag` to nil via
`M-x customize-variable`.

### racket-align
<kbd>M-x racket-align</kbd>

Align values in the same column.

Useful for binding forms like `let` and `parameterize`,
conditionals like `cond` and `match`, association lists, and any
series of couples like the arguments to `hash`.

Before choosing this command, put point on the first of a series
of "couples". A couple is:

- A list of two or more sexprs: `[sexpr val sexpr ...]`
- Two sexprs: `sexpr val`.

Each `val` moves to the same column and is
`prog-indent-sexp`-ed (in case it is a multi-line form).

For example with point on the `[` before `a`:

    Before             After

    (let ([a 12]       (let ([a   12]
          [bar 23])          [bar 23])
      ....)              ....)

    '([a . 12]         '([a   . 12]
      [bar . 23])        [bar . 23])

    (cond [a? #t]      (cond [a?   #t]
          [b? (f x           [b?   (f x
                 y)]                  y)]
          [else #f])         [else #f])

Or with point on the `'` before `a`:

    (list 'a 12        (list 'a   12
          'bar 23)           'bar 23)

If more than one couple is on the same line, none are aligned,
because it is unclear where the value column should be. For
example the following form will not change; [`racket-align`](#racket-align) will
display an error message:

    (let ([a 0][b 1]
          [c 2])       error; unchanged
      ....)

When a couple's sexprs start on different lines, that couple is
ignored. Other, single-line couples in the series are aligned as
usual. For example:

    (let ([foo         (let ([foo
           0]                 0]
          [bar 1]            [bar 1]
          [x 2])             [x   2])
      ....)              ....)

See also: [`racket-unalign`](#racket-unalign).

### racket-unalign
<kbd>M-x racket-unalign</kbd>

The opposite of [`racket-align`](#racket-align).

Effectively does M-x `just-one-space` and `prog-indent-sexp` for
each couple's value.

### racket-complete-at-point
Default value for the variable `completion-at-point-functions`.

Completion candidates are drawn from the namespace symbols
resulting from the most recent [`racket-run`](#racket-run) of each .rkt file. If
a file has never been run, candidates default to values also used
for font-lock -- an assortment of symbols from common Racket
modules such as `racket`, `typed/racket`, and `syntax/parse`.

Returns extra :company-doc-buffer and :company-location
properties for use by the `company-mode` backend `company-capf`
-- but not :company-docsig, because it is frequently impossible
to supply this quickly enough or at all.

## Macro expand

### racket-stepper-mode
<kbd>M-x racket-stepper-mode</kbd>

Major mode for Racket stepper output.

Used by the commands [`racket-expand-file`](#racket-expand-file),
[`racket-expand-definition`](#racket-expand-definition), [`racket-expand-region`](#racket-expand-region), and
[`racket-expand-last-sexp`](#racket-expand-last-sexp).



```
key             binding
---             -------

RET		racket-stepper-step
j		racket-stepper-next-item
k		racket-stepper-previous-item
n		racket-stepper-next-item
p		racket-stepper-previous-item


```


In addition to any hooks its parent mode `special-mode` might have run,
this mode runs the hook [`racket-stepper-mode-hook`](#racket-stepper-mode-hook), as the final step
during initialization.

### racket-expand-file
<kbd>C-c C-e f</kbd>

Expand the [`racket-mode`](#racket-mode) buffer's file in [`racket-stepper-mode`](#racket-stepper-mode).

Uses the `macro-debugger` package to do the expansion.

You do _not_ need to [`racket-run`](#racket-run) the file first; the namespace
active in the REPL is not used.

If the file is non-trivial and/or is not compiled to a .zo
bytecode file, then it might take many seconds before the
original form is displayed and you can start stepping.

With a prefix, also expands syntax from racket/base -- which can
result in very many expansion steps.

### racket-expand-region
<kbd>C-c C-e r</kbd>

Expand the active region using [`racket-stepper-mode`](#racket-stepper-mode).

Uses Racket's `expand-once` in the namespace from the most recent
[`racket-run`](#racket-run).

### racket-expand-definition
<kbd>C-c C-e x</kbd>

Expand the definition around point using [`racket-stepper-mode`](#racket-stepper-mode).

Uses Racket's `expand-once` in the namespace from the most recent
[`racket-run`](#racket-run).

### racket-expand-last-sexp
<kbd>C-c C-e e</kbd>

Expand the sexp before point using [`racket-stepper-mode`](#racket-stepper-mode).

Uses Racket's `expand-once` in the namespace from the most recent
[`racket-run`](#racket-run).

## Other

### racket-mode-start-faster
<kbd>M-x racket-mode-start-faster</kbd>

Compile racket-mode's .rkt files for faster startup.

racket-mode is implemented as an Emacs Lisp "front end" that
talks to a Racket process "back end". Because racket-mode is
delivered as an Emacs package instead of a Racket package,
installing it does _not_ do the `raco setup` that is normally
done for Racket packages.

This command will do a `raco make` of racket-mode's .rkt files,
creating bytecode files in `compiled/` subdirectories. As a
result, when a [`racket-run`](#racket-run) or [`racket-repl`](#racket-repl) command must start
the Racket process, it will start faster.

If you run this command, _ever_, you should run it _again_ after:

- Installing an updated version of racket-mode. Otherwise, you
  might lose some of the speed-up.

- Installing a new version of Racket and/or changing the value of
  the variable [`racket-program`](#racket-program). Otherwise, you might get an
  error message due to the bytecode being different versions.

# Variables

> Note: You may also set these via Customize.

## General

### racket-program
Pathname of the racket executable.

### racket-command-port
Port number for Racket REPL command server.

### racket-command-startup
What to do when the REPL and command server aren't available to send a command.

- nil: Show an error message explaining that you might need to
  start or restart the Racket REPL.

- positive number: Automatically try to start the REPL and wait
  that number of seconds for command server to become
  available.

### racket-command-timeout
How many seconds to wait for Racket REPL command server responses.

### racket-memory-limit
Terminate the Racket process if memory use exceeds this value in MB.
Changes to this value take effect upon the next [`racket-run`](#racket-run). A value
of 0 means no limit.

Caveat: This uses Racket's `custodian-limit-memory`, which does
not enforce the limit exactly. Instead, the program will be
terminated upon the first garbage collection where memory exceeds
the limit (maybe by a significant amount).

### racket-error-context
The level of context used for [`racket-run`](#racket-run) error stack traces.

Each level improves stack trace information, but causes your
program to run more slowly.

  - 'low corresponds to `compile-context-preservation-enabled`
    `#f`.

  - 'medium corresponds to `compile-context-preservation-enabled`
    `#t`, which disables some optimizations like inlining.

  - 'high corresponds to `compile-context-preservation-enabled`
    `#t` and to use of `errortrace`, which heavily instruments
    your code and therefore may be significantly slower.

Tip: Regardless of this setting, you can enable 'high errortrace
for a specific [`racket-run`](#racket-run) using a C-u prefix. This lets you
normally run with a faster setting, and temporarily re-run to get
a more-helpful error message.

### racket-retry-as-skeleton
Retry a "skeleton" of files with errors, for identifier names?

When true: If your source file has an error, a "skeleton" of
your file is evaluated to get identifiers from module languages,
`require`s, and definitions. That way, things like completion and
[`racket-describe`](#racket-describe) are more likely to work while you edit the file
to fix the error.

Otherwise, you'll have only identifiers provided by racket/base,
until you fix the error and run again.

You might want to disable this if you work with files that take a
very long time to expand -- because this feature needs to expand
again when there is an error.

### racket-user-command-line-arguments
List of command-line arguments to supply to your Racket program.

Accessible in your Racket program in the usual way -- the
parameter `current-command-line-arguments` and friends.

This is an Emacs buffer-local variable -- convenient to set as a
file local variable. For example at the end of your .rkt file:

    ;; Local Variables:
    ;; racket-user-command-line-arguments: ("-f" "bar")
    ;; End:

Set this way the value must be an unquoted list of strings such
as:

    ("-f" "bar")

but NOT:

    '("-f" "bar")
    (list "-f" "bar")


### racket-path-from-emacs-to-racket-function
A function used to transform Emacs Lisp pathnames before supplying to the Racket back end.

If you run Emacs on Windows Subsystem for Linux, and want to run
Racket programs using Windows Racket.exe rather than Linux
racket, you can set this to [`racket-wsl-to-windows`](#racket-wsl-to-windows). In that case
you probably also want to customize the "reverse":
[`racket-path-from-racket-to-emacs-function`](#racket-path-from-racket-to-emacs-function).

### racket-path-from-racket-to-emacs-function
A function used to transform pathnames supplied by the Racket back end before using them in Emacs.

The default on Windows replaces back with forward slashes. The
default elsewhere is `identity`.

If you run Emacs on Windows Subsystem for Linux, and want to run
Racket programs using Windows Racket.exe rather than Linux
racket, you can set this to [`racket-windows-to-wsl`](#racket-windows-to-wsl). In that case
you probably also want to customize the "reverse":
[`racket-path-from-emacs-to-racket-function`](#racket-path-from-emacs-to-racket-function).

## REPL

### racket-history-filter-regexp
Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters.

### racket-images-inline
Whether to display inline images in the REPL.

### racket-images-keep-last
How many images to keep in the image cache.

### racket-images-system-viewer
Which system image viewer program to invoke upon M-x
 [`racket-view-last-image`](#racket-view-last-image).

### racket-pretty-print
Use pretty-print instead of print in REPL.

## Other

### racket-indent-curly-as-sequence
Indent `{}` with items aligned with the head item?
This is indirectly disabled if [`racket-indent-sequence-depth`](#racket-indent-sequence-depth) is 0.
This is safe to set as a file-local variable.

### racket-indent-sequence-depth
To what depth should [`racket-indent-line`](#racket-indent-line) search.
This affects the indentation of forms like `` '()` `() #() `` --
and `{}` if [`racket-indent-curly-as-sequence`](#racket-indent-curly-as-sequence) is t -- but not
`` #'() #`() ,() ,@() ``. A zero value disables, giving the
normal indent behavior of DrRacket or Emacs `lisp-mode` derived
modes like `scheme-mode`. Setting this to a high value can make
indentation noticeably slower. This is safe to set as a
file-local variable.

### racket-pretty-lambda
Display lambda keywords using λ. This is DEPRECATED.
Instead use `prettify-symbols-mode` in newer verisons of Emacs,
or, use [`racket-insert-lambda`](#racket-insert-lambda) to insert actual λ characters.

### racket-smart-open-bracket-enable
Use [`racket-smart-open-bracket`](#racket-smart-open-bracket) when `[` is pressed?

### racket-logger-config
Configuration of [`racket-logger-mode`](#racket-logger-mode) topics and levels

The topic '* respresents the default level used for topics not
assigned a level. Otherwise, the topic symbols are the same as
used by Racket's `define-logger`.

The levels are those used by Racket's logging system: 'debug,
'info, 'warning, 'error, 'fatal.

For more information see:
  <https://docs.racket-lang.org/reference/logging.html>

The default value sets some known "noisy" topics to be one
level quieter. That way you can set the '* topic to a level like
'debug and not get overhwelmed by these noisy topics.

## Experimental debugger

### racket-debuggable-files
Used to tell [`racket-run`](#racket-run) what files may be instrumented for debugging.
Must be a list of strings that are pathnames, such as from
[`racket--buffer-file-name`](#racket--buffer-file-name), -or-, a function that returns such a
list given the pathname of the file being run. If any path
strings are relative, they are made absolute using
`expand-file-name` with the directory of the file being run. The
symbol 'run-file may be supplied in the list; it will be replaced
with the pathname of the file being run. Safe to set as a
file-local variable.

# Faces

> Note: You may also set these via Customize.

### racket-keyword-argument-face
Face for `#:keyword` arguments.

### racket-selfeval-face
Face for self-evaluating expressions like numbers, symbols, strings.

### racket-here-string-face
Face for here strings.

### racket-check-syntax-def-face
Face [`racket-check-syntax`](#racket-check-syntax) uses to highlight definitions.

### racket-check-syntax-use-face
Face [`racket-check-syntax`](#racket-check-syntax) uses to highlight uses.

### racket-logger-config-face
Face for [`racket-logger-mode`](#racket-logger-mode) configuration.

### racket-logger-topic-face
Face for [`racket-logger-mode`](#racket-logger-mode) topics.

### racket-logger-fatal-face
Face for [`racket-logger-mode`](#racket-logger-mode) fatal level.

### racket-logger-error-face
Face for [`racket-logger-mode`](#racket-logger-mode) error level.

### racket-logger-warning-face
Face for [`racket-logger-mode`](#racket-logger-mode) warning level.

### racket-logger-info-face
Face for [`racket-logger-mode`](#racket-logger-mode) info level.

### racket-logger-debug-face
Face for [`racket-logger-mode`](#racket-logger-mode) debug level.

