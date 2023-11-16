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
          ("C-k" . corfu-previous)))
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :config
  (set-face-attribute 'corfu-border nil :inherit 'region :background 'unspecified)
  (with-eval-after-load 'doom-themes
    (set-face-background 'corfu-current (doom-color 'dark-blue)))
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-preview-current nil
        corfu-auto-delay 0.1
        corfu-popupinfo-delay '(0.2 . 0.1)))

(use-package corfu-popupinfo
  :defer t
  :ensure corfu
  :bind (:map corfu-popupinfo-map
         ("C-h" . corfu-popupinfo-toggle)
         ("C-d" . corfu-popupinfo-scroll-up)
         ("C-b" . corfu-popupinfo-scroll-down)
         ("gg" . corfu-popupinfo-beginning)
         ("G" . corfu-popupinfo-end)))

(unless (display-graphic-p)
  (use-package corfu-terminal
    :hook (global-corfu-mode . corfu-terminal-mode)))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))


(provide 'init-corfu)

;;; init-corfu.el ends here
