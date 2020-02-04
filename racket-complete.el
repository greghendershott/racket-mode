;;; racket-complete.el -*- lexical-binding: t -*-

;; Copyright (c) 2013-2020 by Greg Hendershott.
;; Portions Copyright (C) 1985-1986, 1999-2013 Free Software Foundation, Inc.

;; Author: Greg Hendershott
;; URL: https://github.com/greghendershott/racket-mode

;; License:
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose. See the GNU
;; General Public License for more details. See
;; http://www.gnu.org/licenses/ for details.

(require 'cl-lib)
(require 'ido)
(require 'racket-custom)
;;(require 'racket-repl)
(require 'racket-keywords-and-builtins)
(require 'racket-describe)
(require 'racket-visit)
(require 'racket-util)

;; TODO:
;;
;; More work making racket-mode into a "plain edit mode" that works
;; without a REPL. People enable the racket-check-syntax-mode (better
;; name like "racket-ide-mode"?) for that stuff.
;;
;; 1. Make racket-mode completion only use the font-lock symbols list
;; not the command server. If people want more accurate completion,
;; they need to use racket-check-syntax-mode, which can do this
;; without needing to run the file.
;;
;; 2. racket-repl-mode should have its own completion function that
;; does use `(syms)` command for live namespace symbols.
;;
;; 3. Of course move the code that determines the "prefix" in the
;; buffer, to some common shared file. (racket-check-sytnax-mode
;; already has a copypasta of this. We don't need/want three
;; instances.)
;;
;; 4. Similar story for visit-definition. racket-mode shouldn't have
;; this command at all. Instead racket-check-syntax-mode supplies it.
;; And, racket-repl-mode may provide this in the old status quo way
;; (i.e. effectively move the code from here to racket-repl.el).
;;
;; 5. The CAPF :company-location thing? I don't have a great story for
;; that in a "plain" racket-mode. :( Even with
;; racket-check-syntax-mode, we don't supply that information -- we
;; don't proactively fetch source location for all imported symbols!
;; Way too slow. Is there some clever hack where we, idk, insert that
;; symbol in a copy of the current buffer, run check-syntax again, and
;; use that annotation? Again the idea is that we don't need to _run_
;; the file, just expand it. Maybe we can expose _that_ expansion-only
;; flavor in the server, not even needing to use
;; drracket/check-syntax.
;;
;; 6. Ditto eldoc-mode. Of course they can still work in the status
;; quo way for racket-repl-mode.
;;
;; 7. racket-describe: This is a mix. The "visit" and "doc" links can
;; be supplied instead by racket-check-syntax-mode annotations. I'm
;; not sure what to do about the stuff where it uses the ,type command
;; because no Scribble doc available. And again, no problem in
;; racket-repl-mode.
;;
;; 8. Consider renaming racket-check-syntax-mode to something like
;; racket-ide-mode. Then make racket-mode work 100% without any REPL
;; -- features people can use if they only want to edit, and will run
;; their code some other way. Any other functionality is supplied by
;; either racket-ide-mode (only needs the server, not the file to
;; racket-run) or racket-repl-mode. Both of these minor modes can
;; override keys, and add things like completion-at-point-functions,
;; to enhance the simpler racket-mode.

(defun racket--call-with-completion-prefix-positions (proc)
  (let ((beg (save-excursion (skip-syntax-backward "^-()>") (point))))
    (unless (or (eq beg (point-max))
                (member (char-syntax (char-after beg)) '(?\" ?\( ?\))))
      (condition-case nil
          (save-excursion
            (goto-char beg)
            (forward-sexp 1)
            (let ((end (point)))
              (and
               (<= (+ beg 2) end) ;prefix at least 2 chars
               (funcall proc beg end))))
        (scan-error nil)))))

(provide 'racket-complete)

;; racket-complete.el ends here
