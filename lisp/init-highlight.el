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
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t) ;; Dont know why this doesn't work
  (setq show-paren-when-point-in-periphery t)
  (defun show-paren-function-advice (fn)
    "Highlight enclosing parens."
    (cond ((looking-at-p "\\s(") (funcall fn))
          (t (save-excursion
               (ignore-errors (backward-up-list))
               (funcall fn)))))
  (advice-add 'show-paren-function :around #'show-paren-function-advice))

;; Similar to show-paren-mode
;; (use-package highlight-parentheses
;;   :ensure t
;;   :hook (after-init . global-highlight-parentheses-mode))

;; Color String
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook ((web-mode
          java-mode
          lisp-mode
          emacs-lisp-mode
          python-mode
          org-mode
          help-mode)
         . rainbow-mode))

;; Highlights delimiters such as parentheses, brackets or braces according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Poor Performance
;; Highlight uncommitted changes
;; (use-package diff-hl
;;   :ensure t
;;   :hook ((after-init . global-diff-hl-mode)
;;          (dired-mode . diff-hl-dired-mode))
;;   :config
;;   (diff-hl-flydiff-mode)
;;   (with-eval-after-load 'magit
;;     (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
;;   ;; There's no fringe when Emacs is running in the console,
;;   ;; but the navigation and revert commands still work.
;;   ;; Consider turning diff-hl-margin-mode on, to show the indicators in the margin instead.
;;   (unless (display-graphic-p)
;;     (diff-hl-margin-mode))
;;   )

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'init-highlight)

;;; init-highlight.el ends here
