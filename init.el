;; init.el --- init Emacs	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  init.el
;;

;;; Code:

;; NOTE: Startup Summary: Sequence of Actions at Startup
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#Startup-Summary

(setq debug-on-error t)

;; Emacs Version
(let ((minver "30"))
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
;; https://emacsconf.org/2023/talks/gc/, https://zenodo.org/records/10518083
;; NOTE: Check `gcs-done' and `gc-elapsed' right after Emacs startup
(setq gc-cons-threshold 80000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (message "[info] gcs-done %d times, gc-elapsed %.03f seconds" gcs-done gc-elapsed)
            (setq gc-cons-threshold 1800000
                  gc-cons-percentage 0.2)))

;; Load Path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; benchmark require times
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-benchmarking.el
(require 'init-benchmarking)

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
(require 'init-dired)
(require 'init-org)
(require 'init-shell-term)
(require 'init-editor)
(require 'init-docker)

;; Completion in minibuffer
(require 'init-minibuffer)

;; Programing
(require 'init-project)
;; (require 'init-completion)
(require 'init-corfu)
(require 'init-yasnippet)
(require 'init-diagnostic)
(require 'init-git)
(require 'init-highlight)

;; Language
(require 'init-eglot)
(require 'init-debugger)
(require 'init-lang-emacs-lisp)
(require 'init-lang-c)
(require 'init-lang-python)
(require 'init-lang-java)
(require 'init-lang-js)
(require 'init-lang-rust)
(require 'init-lang-go)
(require 'init-lang-clojure)
(require 'init-lang-lua)
(require 'init-lang-sql)
(require 'init-web)

;; Misc
(require 'init-file-modes)
(require 'init-misc)
(require 'init-window)
(require 'init-workspace)
(require 'init-telega)
(require 'init-ai)

(add-hook 'emacs-startup-hook (lambda () (setq debug-on-error nil)))


(provide 'init)

;;; init.el ends here
