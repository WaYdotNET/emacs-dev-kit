;;; common configuration for most programming modes
;;; This file is part of the Emacs Dev Kit

;; this will get applied to all modes extending cc-mode
;; like java-mode, php-mode, etc
(defun c-coding-hook ()
  (setq c-basic-offset 4)
  (coding-hook))

(add-hook 'c-mode-common-hook 'c-coding-hook)

;; Lisp configuration
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

;; a great lisp coding hook
(defun lisp-coding-hook ()
  (coding-hook)
  (setq autopair-dont-activate t)
  (paredit-mode +1)
)

;; interactive modes don't need whitespace checks
(defun interactive-lisp-coding-hook ()
  (setq autopair-dont-activate t)
  (paredit-mode +1)
  (turn-off-whitespace))

;; sr-speedbar
(require 'sr-speedbar)
(speedbar-add-supported-extension ".rb")
(speedbar-add-supported-extension ".ru")
(speedbar-add-supported-extension ".yaml")
(speedbar-add-supported-extension ".yml")
(speedbar-add-supported-extension ".css")
(speedbar-add-supported-extension ".sass")
(speedbar-add-supported-extension ".scss")
(speedbar-add-supported-extension ".haml")
(speedbar-add-supported-extension ".feature")
(speedbar-add-supported-extension ".config")
(speedbar-add-supported-extension "Gemfile")
(speedbar-add-supported-extension "Rakefile")
(speedbar-add-supported-extension ".erb")
(speedbar-add-supported-extension ".textile")
(speedbar-add-supported-extension ".markdown")
(speedbar-add-supported-extension ".less")
(speedbar-add-supported-extension ".slim")
(speedbar-add-supported-extension ".tt")

(sr-speedbar-refresh-turn-off)

(provide 'coding-config)
