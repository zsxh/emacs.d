;; init-corfu.el --- corfu.el configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  corfu.el configurations
;;

;;; Code:

(use-package corfu
  :bind (("M-/" . completion-at-point)
         (:map corfu-map
          ("C-j" . corfu-next)
          ("C-k" . corfu-previous))
         (:map corfu-popupinfo-map
          ("C-h" . corfu-popupinfo-toggle)
          ("C-d" . corfu-popupinfo-scroll-up)
          ("C-b" . corfu-popupinfo-scroll-down)))
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
         (org-mode . corfu-mode)
         (markdown-mode . corfu-mode))
  :config
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.1
        corfu-on-exact-match nil
        corfu-preview-current nil
        corfu-popupinfo-delay '(1.0 . 0.5))
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :hook (global-corfu-mode . corfu-terminal-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))


(provide 'init-corfu)

;;; init-corfu.el ends here
