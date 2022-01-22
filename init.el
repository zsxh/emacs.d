;; init.el --- init Emacs	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  init.el
;;

;;; Code:

(setq debug-on-error t)

;; Emacs Version
(let ((minver "28"))
  (when (version< emacs-version minver)
    (error "Detected Emacs %s. This config requires v%s or higher" emacs-version minver)))

(when (display-graphic-p)
  ;; NOTE: have issue when in terminal
  ;; Speedup boot time by unset file-name-handler-alist temporarily
  ;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#unset-file-name-handler-alist-temporarily
  (defvar tmp--file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist tmp--file-name-handler-alist))))

;; Speedup Boostrap
;; Adjust garbage collection thresholds during startup, and thereafter
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#avoid-garbage-collection-at-startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook (lambda ()
                                "Restore defalut values after startup."
                                (setq gc-cons-threshold 16777216 ; 16mb
                                      gc-cons-percentage 0.1)))
;; Load Path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Config
(require 'init-config)
(require 'init-custom)

;; Package Configuations
(require 'init-package)

;; UI
(require 'init-ui)

;; Emacs environment
(require 'init-exec-path)

;; Load custom functions
(require 'init-funcs)

;; KeyBinding
(require 'init-evil)
(require 'init-keybinding)

;; Feature
(require 'init-emacs-enhancement)
(require 'init-neotree)
(require 'init-org)
(require 'init-shell-term)
(require 'init-editor)
(require 'init-docker)

;; Completion in minibuffer
(require 'init-minibuffer)

;; Programing
(require 'init-project)
(require 'init-completion)
(require 'init-syntax-checking)
(require 'init-git)
(require 'init-highlight)
(require 'init-prog)
(require 'init-lsp)
(require 'init-debugger)

;; Language
(require 'init-lang-emacs-lisp)
(require 'init-lang-common-lisp)
(require 'init-lang-c)
(require 'init-lang-python)
(require 'init-lang-java)
(require 'init-lang-julia)
(require 'init-lang-js)
(require 'init-lang-rust)
(require 'init-lang-go)
(require 'init-lang-clojure)
(require 'init-lang-sql)
(require 'init-web)
(require 'init-jupyter)
;; (require 'init-latex)

;; Misc
(require 'init-eaf)
(require 'init-misc)
(require 'init-window)

(add-hook 'emacs-startup-hook (lambda () (setq debug-on-error nil)))


(provide 'init)

;;; init.el ends here
