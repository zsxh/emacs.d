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
  :hook ((lisp-data-mode clojure-mode) . show-paren-local-mode)
  :config
  (setq show-paren-mode nil
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        show-paren-delay 0.125))

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
  :hook (lisp-data-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  ;; :if (not IS-LINUX)
  ;; :hook ((python-mode python-ts-mode yaml-mode yaml-ts-mode docker-compose-mode) . highlight-indent-guides-mode)
  :defer t
  :config
  (defun my-highlighter (level responsive display)
    (if (> 1 level)
        nil
      (highlight-indent-guides--highlighter-default level responsive display)))

  (setq highlight-indent-guides-method 'bitmap
        ;; highlight-indent-guides-character ?\|
        highlight-indent-guides-auto-character-face-perc 30
        highlight-indent-guides-auto-top-character-face-perc 60
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-highlighter-function 'my-highlighter))

;; Fast, configurable indentation guide-bars for Emacs
;; For indent-bars to work, your port and version of emacs must correctly display the `:stipple' face attribute.
;; https://github.com/jdtsmith/indent-bars
(use-package indent-bars
  :if IS-LINUX
  :vc (:url "https://github.com/jdtsmith/indent-bars" :rev :newest)
  :hook ((python-mode python-ts-mode yaml-mode yaml-ts-mode docker-compose-mode) . indent-bars-mode))

(with-eval-after-load 'compile
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

;; highlight comment tags like TODO, BUG, FIXME, etc.
(use-package hl-todo
  :hook ((prog-mode yaml-mode yaml-ts-mode) . hl-todo-mode)
  :config
  (push '("WIP" . "#7cb8bb") hl-todo-keyword-faces))

;; flash the line the cursor is on
(use-package pulse
  :ensure nil
  :defer t
  :config
  (set-face-background 'pulse-highlight-start-face "#51afef")
  (when (fboundp 'set-face-extend)
    (set-face-extend 'pulse-highlight-start-face t)))

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
