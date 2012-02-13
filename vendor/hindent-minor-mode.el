;;;
;;; Copyright (C) Kresten Krab Thorup
;;; Available under Apache License, Version 2.
;;;

;;;
;;; This minor mode will highlight the indentation of the current line
;;; as a vertical bar (grey background color) aligned with the column of the
;;; first character of the current line.
;;;

(defface hindent-face
  ;; Fringe has non intrusive color in most color-themes
  '((t :inherit fringe))
  "Basic face for highlighting indentation guides."
  :group 'basic-faces)

(define-minor-mode
  hindent-minor-mode
  "Hilight Indentation minor mode displays
a vertical bar corresponding to the indentation of the current line"
  :lighter " |"

  (if hindent-current-regex
      (font-lock-remove-keywords nil hindent-current-regex))

  (set (make-local-variable 'hindent-current-regex) nil)
  (if hindent-minor-mode
      (add-hook 'post-command-hook 'hindent-current-line-hook nil t)
    (font-lock-fontify-buffer)))

;; used to hold the last regex we installed
(defvar hindent-current-regex nil)

;;
;; This hook runs after every keystroke
;;
(defun hindent-current-line-hook ()

  (progn
  ;;    (display-warning :warning "inside hook")

  (let* ((current-point (point))
         (indent (progn
                   (beginning-of-line)
                   (skip-chars-forward " \t")
                   (current-column))))

    (progn
      (if hindent-current-regex
        (font-lock-remove-keywords nil hindent-current-regex))

      (if (and hindent-minor-mode
               (> indent 1))
          (let* ((re (format "^.\\{%d\\}\\( \\)" indent))
                 (arg `((,re (1 'hindent-face prepend)))))
            (progn
              (set (make-local-variable 'hindent-current-regex) arg)
              (font-lock-add-keywords nil arg)))
        (remove-hook 'post-command-hook 'highlight-ident-current-line-hook t))
      (font-lock-fontify-buffer)
      (goto-char current-point)))))

(provide 'hindent-minor-mode)
