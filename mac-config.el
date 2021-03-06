;;; OS X specific setings
;;; This file is part of the Emacs Dev Kit

;; Emacs users obviously have little need for Command and Option keys,
;; but they do need Meta and Control
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'control)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(provide 'mac-config)
