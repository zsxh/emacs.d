;; init.el --- init Emacs	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  intel.el
;;

;;; Code:

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
      (larger-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold larger-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda ()
              "Restore defalut values after startup."
              (setq gc-cons-threshold normal-gc-cons-threshold)

              ;; GC automatically while unfocusing the frame
              ;; `focus-out-hook' is obsolete since 27.1
              (if (boundp 'after-focus-change-function)
                  (add-function :after after-focus-change-function
                                (lambda ()
                                  (unless (frame-focus-state)
                                    (garbage-collect))))
                (add-hook 'focus-out-hook 'garbage-collect))

              ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
              ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
              (defun my-minibuffer-setup-hook ()
                (setq gc-cons-threshold larger-gc-cons-threshold))

              (defun my-minibuffer-exit-hook ()
                (garbage-collect)
                (setq gc-cons-threshold normal-gc-cons-threshold))

              (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
              (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook))))

;; Load Path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(let ((base (expand-file-name "submodules" user-emacs-directory)))
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; Customization
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

;; Completion in Emacs
(require 'init-ivy)

;; Programing
(require 'init-project)
(require 'init-completion)
(require 'init-syntax-checking)
(require 'init-git)
(require 'init-highlight)
(require 'init-prog)
(require 'init-language-server)
(require 'init-debugger)

;; Language
(require 'init-emacs-lisp)
(require 'init-common-lisp)
(require 'init-c)
(require 'init-python)
(require 'init-java)
(require 'init-web)
(require 'init-julia)
(require 'init-js)
(require 'init-rust)
(require 'init-go)
(require 'init-jupyter)

;; Experimental
(require 'init-experimental)

;; Misc
(require 'init-misc)

(add-hook 'emacs-startup-hook (lambda () (setq debug-on-error nil)))

(provide 'init)

;;; init.el ends here
