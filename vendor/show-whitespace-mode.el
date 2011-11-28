;;; show-whitespace.el --- Simple mode to highlight whitespaces

;; Copyright (C) 2002 by Aurélien Tisné
;; Author: Aurélien Tisné <aurelien.tisne@c-s.fr>
;; Keywords: convenience
;; Created: 7 Aug 2002

;; This file is not part of GNU Emacs.

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Introduction:
;;
;; Whitespaces (spaces and tabs) are not visible by default. This package,
;; when it is activated, highlights spaces and tabs. It may be useful to see
;; trailing whitespaces (and to decide to use 'delete-trailing-whitespace').
;; Highlight rules can be easily change to fit your need (specific highlights
;; for leading and trailing spaces for instance).

;; Usage:
;;
;; Go to the appropriate buffer and press:
;;   M-x show-whitespace-mode RET

;; The function `turn-on-show-whitespace-mode' could be added to any major
;; mode hook to activate Show-Whitespace Mode for all buffers in that
;; mode.  For example, the following line will activate Show-Whitespace
;; Mode in all SGML mode buffers:
;;
;; (add-hook 'sgml-mode-hook 'turn-on-show-whitespace-mode)

;; Support:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;
;;  This version of show-whitespace was developed with NTEmacs 21.1.1 
;;  under MS Windows XP.
;;  Please, let me know if it works with other OS and versions of Emacs.

;;; Code:

(defgroup show-whitespace nil
  "Minor mode to show whitespaces."
  :prefix "show-ws-"
  :group 'editing
)

;; Variables:

(defvar show-ws-font-lock-keywords-back nil 
  "Used to save initial font-lock-keywords value to be able to restore
it if the mode is switched off.")
(make-local-variable 'show-ws-font-lock-keywords-back)

;; Faces:

(defface show-ws-spaces
  `((((type tty) (class color))
     (:background "LemonChiffon1"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color))
     (:background "LemonChiffon1"))
    (t (:background "LemonChiffon1")))
  "Face for highlighting spaces."
  :group 'show-whitespace)

(defface show-ws-tabs
  `((((type tty) (class color))
     (:background "LemonChiffon2"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color))
     (:background "LemonChiffon2"))
    (t (:background "LemonChiffon2")))
  "Face for highlighting tabs."
  :group 'show-whitespace)


;; Functions:

;;;###autoload
(define-minor-mode show-whitespace-mode
  "Toggle whitespace highlighting in current buffer.

With arg, turn Show-Whitespace mode on if and only if arg is positive.
This is a minor mode that affects only the current buffer."
  ;; the initial value
  nil
  ;; the indicator for the mode line
  " show-ws"
  ;; the keymap
  nil
  ;; the body
  (if show-whitespace-mode
      ;; Show whitespaces distinguishing spaces and tabs
      (progn
        (setq show-ws-font-lock-keywords-back font-lock-keywords)
        (setq font-lock-keywords
              (append font-lock-keywords
                      '(
                        ;; show spaces
                        ("[ ]+"   (0 'show-ws-spaces t))
                        ;; show tabs
                        ("[\t]+" (0 'show-ws-tabs t))
                        ))))
    (setq font-lock-keywords show-ws-font-lock-keywords-back))
  (font-lock-fontify-buffer))

;;;###autoload
(defun turn-on-show-whitespace-mode ()
  "Turn on Show-Whitespace Mode.

This function is designed to be added to hooks, for example:
  (add-hook 'c-mode-hook 'turn-on-show-whitespace-mode)"
  (show-whitespace-mode 1))


;;  Allow this feature to be used.
(provide 'show-whitespace-mode)

;; show-whitespace.el ends here.
