;; init-highlight.el --- Highlight EveryThing	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Highlighting EveryThing
;;

;;; Code:

;; Highlight matching parenthesis
(use-package paren
  :ensure nil
  :hook ((after-init . (lambda () (show-paren-mode -1)))
         (prog-mode . show-paren-local-mode))
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)

  (defun show-paren-function-advice (fn)
    "Highlight enclosing parens."
    (cond ((looking-at-p "\\s(") (funcall fn))
          ((derived-mode-p 'python-mode)
           (save-excursion
             (ignore-errors
               (let* ((cur-pos (point))
                      (paren-open-pos (search-backward-regexp "\\s(" (point-min) t))
                      (paren-close-pos (and paren-open-pos (search-forward-regexp "\\s)" cur-pos t))))
                 (when (and paren-open-pos (not paren-close-pos))
                   (goto-char (1+ paren-open-pos))
                   (funcall fn))))))
          (t (save-excursion
               (ignore-errors (backward-up-list))
               (funcall fn)))))

  (advice-add 'show-paren-function :around #'show-paren-function-advice)

  (defun show-paren-local-mode ()
    (interactive)
    (setq-local show-paren-mode t)))

;; Highlight Symbol
(use-package symbol-overlay
  :defer t
  :config
  (defun symbol-overlay-goto-first ()
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol))
           (a-symbol (car keyword))
           (before (symbol-overlay-get-list a-symbol 'car))
           (count (length before)))
      (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count))))

  (defun symbol-overlay-goto-last ()
    (interactive)
    (let* ((symbol (symbol-overlay-get-symbol))
           (keyword (symbol-overlay-assoc symbol))
           (a-symbol (car keyword))
           (after (symbol-overlay-get-list a-symbol 'cdr))
           (count (length after)))
      (symbol-overlay-jump-call 'symbol-overlay-basic-jump (- count 1))))

  (define-key symbol-overlay-map (kbd "<") 'symbol-overlay-goto-first)
  (define-key symbol-overlay-map (kbd ">") 'symbol-overlay-goto-last)
  (define-key symbol-overlay-map (kbd "h") 'nil)
  (define-key symbol-overlay-map (kbd "?") 'symbol-overlay-map-help))

;; Color String
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((web-mode
          lisp-mode
          emacs-lisp-mode
          help-mode
          js-mode
          css-mode)
         . rainbow-mode))

;; Highlights delimiters such as parentheses, brackets or braces according to their depth
(use-package rainbow-delimiters
  :hook ((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :hook ((python-mode) . highlight-indent-guides-mode)
  :config
  (defun my-highlighter (level responsive display)
    (if (> 2 level)
        nil
      (highlight-indent-guides--highlighter-default level responsive display)))

  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|
        highlight-indent-guides-auto-character-face-perc 30
        highlight-indent-guides-auto-top-character-face-perc 60
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-highlighter-function 'my-highlighter))

;; TODO: try xterm-color?
(use-package ansi-color
  :ensure nil
  :commands ansi-color-apply-on-region)

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; highlight comment tags like TODO, BUG, FIXME, etc.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; flash the line the cursor is on
(use-package pulse
  :ensure nil
  :defer t
  :config
  (set-face-background 'pulse-highlight-start-face "#51afef"))

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
                   scroll-down-command
                   recenter-top-bottom
                   other-window
                   winum-select-window-by-number
                   avy-jump
                   avy-goto-line))
  ;; (advice-remove command #'pulse-line)
  (advice-add command :after #'pulse-line))


(provide 'init-highlight)

;;; init-highlight.el ends here
