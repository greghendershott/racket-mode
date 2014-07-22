;;; racket-common.el

;; Copyright (c) 2013-2014 by Greg Hendershott.
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

;; Emacs version compatibility

;; In case not Emacs 24.1+, define equivalent of its
;; `pop-to-buffer-same-window'.
(defun racket-pop-to-buffer-same-window
  (&optional buffer-or-name norecord label)
  "Pop to buffer specified by BUFFER-OR-NAME in the selected window."
  (if (fboundp 'pop-to-buffer-same-window)
      (funcall
       'pop-to-buffer-same-window buffer-or-name norecord)
    (funcall 'switch-to-buffer buffer-or-name norecord)))

;; In case not Emacs 24.3+, define equivalent of its `setq-local'.
(eval-and-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      `(set (make-local-variable ',var) ,val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'racket-emacs-compat)

;; racket-emacs-compat.el ends here
