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

;; disable line numbers in the speedbar frame
(add-to-list 'linum-disabled-modes-list '(speedbar-mode))

;; (sr-speedbar-refresh-turn-off)
;; show all files
(setq speedbar-show-unknown-files t)

;; turn off the ugly icons
(setq speedbar-use-images nil)

;; left-side pane
(setq sr-speedbar-right-side nil)

;; don't refresh on buffer changes
(setq sr-speedbar-auto-refresh nil)

;; nicer fonts for speedbar when in GUI
(when (window-system)
  ;; keep monospace buttons, but smaller height
  (set-face-attribute 'speedbar-button-face nil :height 100)

  ;; change to system default UI font for entries
  (dolist (face (list 'speedbar-file-face 'speedbar-directory-face
                      'speedbar-tag-face 'speedbar-selected-face
                      'speedbar-highlight-face))
    (if (eq system-type 'darwin) ;; Lucida Grande on OS X
        (set-face-attribute face nil :family "Lucida Grande" :height 90)
      (set-face-attribute face nil :family "Droid Sans" :height 90))))

;; no left fringe and half-size right fringe. TODO: doesn't work
(add-to-list 'speedbar-frame-parameters '(left-fringe . 0))

(require 'htmlize)
(require 'tomatinho)
(global-set-key (kbd "<f4>") 'tomatinho)

(provide 'coding-config)
