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
(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Detected Emacs %s. This config requires v%s or higher" emacs-version minver)))

;; Speedup boot time by unset file-name-handler-alist temporarily
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#unset-file-name-handler-alist-temporarily
(defvar tmp--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Speedup Boostrap
;; Adjust garbage collection thresholds during startup, and thereafter
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#avoid-garbage-collection-at-startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook (lambda ()
                                "Restore defalut values after startup."
                                (setq gc-cons-threshold 16777216 ; 16mb
                                      gc-cons-percentage 0.1)
                                (setq file-name-handler-alist tmp--file-name-handler-alist)))

;; Load Path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Pdumper configs
(when (bound-and-true-p personal-dumped-p)
  (setq load-path personal-dumped-load-path)
  (global-font-lock-mode)
  (transient-mark-mode)

  ;; FIXME: magit-section fringe not working
  (setq magit-section-visibility-indicator
        (if (window-system)
            '(magit-fringe-bitmap> . magit-fringe-bitmapv)
          '("…" . t)))

  (setq doom-modeline-icon (display-graphic-p))

  ;; Some packages did not load correctly
  (add-hook 'after-init-hook
            (lambda ()
              (save-excursion
                (switch-to-buffer "*scratch*")
                (lisp-interaction-mode)))))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

;; Emacs startup *scratch* buffer
(setq inhibit-startup-screen t
      initial-buffer-choice  nil)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a notable affect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)

;; Performance on Windows is considerably worse than elsewhere, especially if
;; WSL is involved. We'll need everything we can get.
(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil   ; slightly faster IO
        w32-pipe-read-delay 0              ; faster ipc
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;; Delete files to trash on macOS, as an extra layer of precaution against
;; accidentally deleting wanted files.
(setq delete-by-moving-to-trash IS-MAC)

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
(require 'init-latex)

;; Misc
(require 'init-eaf)
(require 'init-misc)
(require 'init-experimental)

(add-hook 'emacs-startup-hook (lambda () (setq debug-on-error nil)))


(provide 'init)

;;; init.el ends here
