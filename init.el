;; -*- lexical-binding: t -*-

(setq debug-on-error t)

;; Emacs Version
(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old. This config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some funcitonality in this config will be disabled. Please upgrade if possible."))

;; Speedup Boostrap
;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook (lambda ()
                               (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Load Path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Package Configuations
(require 'init-package)

;; UI
(require 'init-ui)

;; KeyBinding
(require 'init-evil)
(require 'init-which-key)

;; Feature
(require 'init-ibuffer)
(require 'init-org)

;; Completion in Emacs
(require 'init-ivy)

;; Programing
(require 'init-edit)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-flycheck)
(require 'init-git)
(require 'init-highlight)

;; Load custom functions
(require 'init-funcs)

;; Language
