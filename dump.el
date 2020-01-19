;;-*- lexical-binding: t -*-

;; Load Path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'package)
(require 'init-custom)
(package-initialize)

(setq-local pdump-list '(use-package company org all-the-icons hydra
                          magit magit-todos ivy counsel swiper ibuffer
                          which-key general doom-themes doom-modeline
                          evil evil-collection evil-magit evil-org init-keybinding))

;; evil-collection need it
(setq evil-want-keybinding nil)

(dolist (package pdump-list)
  (require package))

(setq personal-dumped-load-path load-path
      personal-dumped-p t)

;; undo tree will cause segmentation fault
(global-undo-tree-mode -1)

(message "dump: %s" pdump-list)

(dump-emacs-portable personal-dump-file)
