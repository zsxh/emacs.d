;; init-debugger.el --- Debugger Settings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Debugger Settings
;;

;;; Code:

;; [Dape] Debug Adapter Protocol for Emacs
;; NOTE: https://github.com/svaante/dape#supported-debug-adapters
;; NOTE: https://microsoft.github.io/debug-adapter-protocol/overview
;; NOTE: [Mastering Golang Debugging in Emacs, vscode, dape args explain](https://blog.dornea.nu/2024/11/28/mastering-golang-debugging-in-emacs/)
;; NOTE: [golang dape example](https://gitlab.com/skybert/my-little-friends/-/blob/master/emacs/.emacs#L865)
(use-package dape
  :after eglot
  :config
  (setq dape-debug nil)
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode 1))

(use-package dape-toolbar
  :vc (:url "https://github.com/zsxh/dape-toolbar")
  :after dape
  :config
  (dape-toolbar-mode))


(provide 'init-debugger)

;;; init-debugger.el ends here
