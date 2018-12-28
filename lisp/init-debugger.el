;; init-debugger.el --- Debugger Settings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Debugger Settings
;;

;;; Code:

;; Debug/edebug evil keybindings
(with-eval-after-load 'evil-collection
  (with-eval-after-load 'debug
    (evil-collection-init 'debug)
    (evil-define-key 'normal debugger-mode-map (kbd "<return>") 'backtrace-help-follow-symbol))
  (with-eval-after-load 'edebug
    (evil-collection-init 'edebug)))

;; Realgud Configs
(use-package realgud
  :ensure t
  :commands (realgud:gdb realgud:ipdb realgud:jdb)
  :config
  (with-eval-after-load 'evil-collection
    (evil-collection-init 'realgud)))

(provide 'init-debugger)

;;; init-debugger.el ends here
