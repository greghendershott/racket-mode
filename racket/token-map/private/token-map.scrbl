#lang scribble/manual
@(require (for-label token-map/core
                     token-map/nav
                     token-map/seq
                     syntax-color/module-lexer))

@title{Token Maps: Use/implement hash-lang lexers/indenters}

@author{Greg Hendershott}

@section{Introduction}

A token-map is the result of applying a hash-lang supplied
@racket[module-lexer] to a program source string. The resulting tokens
are stored in a data structure that supports quickly finding the token
for a given position, as well as quickly handling incremental updates
to the source string.

A token-map can be used to change the appearance of text in a tool
like a programming editor.

Furthermore, the token-map distinguishes tokens that open and
close "expressions" or "blocks" as defined by the lang's lexer, and
provides functions to "navigate" the revealed structure. Not only is
navigation useful for tools like editors, it can also be used by a
lang-supplied indenter, which often requires such knowledge.

In other words:

@itemlist[

 @item{A hash-lang supplies a lexer.}

 @item{A tool uses this lexer to create a token-map.}

 @item{The token-map is given to a lang's optional indenter function,
       as well as being used by tools like editors.}]

A somewhat non-trivial consideration is being able to update the
token-map quickly --- possibly in response to every single key that a
user types in an editor. Although it might be acceptable to accumulate
contiguous changes in the editor before re-lexing, and/or for there to
be a delay before the screen refreshes with a new color, if the user
presses RETURN the update must be completed immediately for the
indenter. Trying to make the update be lazy but force-able can be
tricky to get correct. It is simpler if the update can be fast enough
to do eagerly. In short, the update should take a minimal amount of
time, as well as yield a minimal set of re-coloring changes for the
editor to perform. (Applying the change list to the editor display
could be done lazily.)

@sidebar{In the spirit of "dog-fooding", I am editing this Scribble
documentation with a prototype Emacs mode that uses this approach.
Also I have used this to try editing large files like the 5000-line
@litchar{drracket/private/unit.rkt} file.}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@defmodule[token-map/core]


@defthing[position/c contract?]{

Checks that a value could be a valid position in a token-map, i.e. an
@racket[exact-positive-integer?].

The minimum position is 1, not 0, because lexers report location spans
that way. (Although some editors, like Emacs, also use 1-based
positions, that is not the main justifiation.)}

@defproc[(token-map? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is token-map, @racket[#f] otherwise.}

@defstruct*[token ([lexeme string?]
                   [backup exact-nonnegative-integer?])]{

A base structure common to all tokens.}

@defstruct*[token:open token ([close string?])]{

A structure representing a token that opens/begins an expression/block
according to the language. The close value is the opposite, matching
close token.}

@defstruct*[token:close token ([open string?])]{

A structure representing a token that opens/begins an expression/block
according to the language. The open value is the opposite, matching
open token.}

@defstruct*[token:misc token ([kind symbol?])]{

A structure representing other kinds of tokens, as determined by the
language's lexer.}

@defstruct*[bounds+token ([beg position/c]
                          [end position/c]
                          [token token?])]{

A structure representing a token and its position interval. The
position is expressed as a half-open interval, @tt{[beg end)} --- that
is, from and including @racket[beg], up to but not including @racket[end].}

@defproc[(create [program-text string?])
         token-map?]{

Create a token-map and populate it using the lexer specified by the
hash-lang line in @racket[program-text]]}

@defproc[(update! [token-map token-map?]
                  [position position/c]
                  [old-length exact-nonnegative-integer?]
                  [new-text string?])
         (listof bounds+token?)]{

Given an update to the program source string, re-tokenize -- doing
minimal work and returning a minimal list of changes. The emphasis
on "minimal" is because potentially this could be called for every
single character change that a user makes while typing in their
editor. (Although you may try to accumulate contiguous changes to call
this, any such tactic is tricky to get right; therefore this seeks to
work as fast as possible.) In any case, this might also be called for
large changes such as a cut or paste.

The function signature here is similar to that of Emacs' after-change
functions:

@itemlist[
 @item{Something changed starting at @racket[position].}
 @item{The old text there used occupied @racket[old-length] characters.}
 @item{The new text there is now @racket[new-text].}]

Insert a new character at position 42: @racket[(update tm 42 0 "A")].

Replace the character at position 42: @racket[(update tm 42 1 "B")].

Delete the character at position 42: @racket[(udpate tm 42 1 "")].}

@defproc[(tokens [token-map token-map?])
         (listof bounds+token?)]{

Return all the bounds+token values in the map.}

@defproc[(classify [token-map token-map?]
                   [position position/c])
         (or/c #f bounds+token?)]{

Return the @racket[bounds+token?] value corresponding to
@racket[position], or @racket[#f].}

@defproc[(classify [token-map token-map?]
                   [position position/c])
         (or/c #f string?)]{

The composition of @racket[classify] with @racket[token-lexme].}

@defproc[(lexer-names [token-map token-map?])
         (listof symbol?)]{

Return a list of all the lexer names used, such as @racket[(list
'racket-lexer)] or @racket[(list 'scribble-inside-lexer)]. This is
provided such that an editor can determine when to replace a
lang-supplied indenter that uses the old, racket/gui-based
drracket:indentation approach, with a token-map alternative such as
@racket[token-map/indent-at-exp]. The idea is that langs ideally would
be updated to a new protocol --- respond to @racket[(get-info
'indent-amount)] by supplying such a new token-map indenter --- but an
editor could have a strategy to support un-updated langs.}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@defmodule[token-map/nav]

@defproc[(beg-of-line [token-map token-map?]
                      [position position/c])
         (or/c #f position/c)]{

Given a position, find the position of the beginning of the line, else
the minimum position, 1.}

@defproc[(end-of-line [token-map token-map?]
                      [position position/c])
         (or/c #f position/c)]{

Given a position, find the position of the end of the line, else the
maximum position.}

@defproc[(backward-up [token-map token-map?]
                      [position position/c])
         (or/c #f position/c)]{

Given a position, find the position of the open token of the enclosing
expression, if any,else the minimum position, 1.}

@defproc[(forward-whitespace [token-map token-map?]
                             [position position/c])
         (or/c #f position/c)]{

Given a position, find the position of the first following token that
is not @racket[token:misc] of kind @racket['white-space]. If there is
no such token, returns #f.}

@defproc[(forward-whitespace/comment [token-map token-map?]
                                     [position position/c])
         (or/c #f position/c)]{

Given a position, find the position of the first following token that
is not @racket[token:misc] of kind @racket[(or 'end-of-line
'white-space 'comment 'sexp-comment)]. If there is no such token,
returns #f.}

@defproc[(backward-whitespace/comment [token-map token-map?]
                                      [position position/c])
         (or/c #f position/c)]{

Given a position, find the position of the first preceding token that
is not @racket[token:misc] of kind 'end-of-line, 'white-space,
'comment, or 'sexp-comment. If there is no such token, returns #f.}

@defproc[(forward-sexp [token-map token-map?]
                       [position position/c]
                       [fail (-> position/c any) (lambda (_pos) #f)])
         (or/c #f position/c)]{

Given a position, find the position of the end of the current
expression, or if already at the end, the end of the following
complete sexp at the same nesting level. If there is no such following
expression, returns the result of applying the failing position to
@racket[fail].}

@defproc[(backward-sexp [token-map token-map?]
                        [position position/c]
                        [fail (-> position/c any) (lambda (_pos) #f)])
         (or/c #f position/c)]{

Given a position, find the position of the beginning of the current
expression, or if already at the beginning, the beginning of the
preceding complete sexp at the same nesting level. If there is no such
preceding expression, returns the result of applying the failing
position to @racket[fail].}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@defmodule[token-map/seq]

@defproc[(in-tokens-forward [token-map token-map?]
                            [position position/c])
         (sequence/c bounds+token?)]{

Return a sequence of tokens from that for @racket[position] forward
through the end.}

@defproc[(in-tokens-backward [token-map token-map?]
                             [position position/c])
         (sequence/c bounds+token?)]{

Return a sequence of tokens from that for @racket[position] backward
through the beginning.}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@defmodule[indent-sexp]

@defproc[(indent-amount [token-map token-map?]
                        [indent-pos position/c])
         exact-nonnegative-integer?]{

An indenter for s-expression languages.}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@defmodule[indent-at-exp]

@defproc[(indent-amount [token-map token-map?]
                        [indent-pos position/c])
         exact-nonnegative-integer?]{

An indenter for at-expression languages.}


@racket[(adsfasfd)]{asdfad asdfasdfadsf}

@racket[(asdf afds
              asdf)]{

asdf adsfadsf afdsafdsdf}
