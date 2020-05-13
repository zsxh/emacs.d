;; early-init.el --- early initial	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;
;; The file is called 'early-init.el', in 'user-emacs-directory'.  It is
;; loaded very early in the startup process: before graphical elements
;; such as the tool bar are initialized, and before the package manager
;; is initialized.  The primary purpose is to allow customizing how the
;; package system is initialized given that initialization now happens
;; before loading the regular init file (see below).
;;
;; We recommend against putting any customizations in this file that
;; don't need to be set up before initializing installed add-on packages,
;; because the early init file is read too early into the startup
;; process, and some important parts of the Emacs session, such as
;; window-system and other GUI features, are not yet set up, which could
;; make some customization fail to work.
;;

;;; Code:

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; make NATIVE_FAST_BOOT=1
;; http://akrl.sdf.org/gccemacs.html#orgac0595f
(setq comp-deferred-compilation t)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. We handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Quickstart: precompute activation actions for faster start up
(setq package-quickstart t)

;; Disable cursor blinking
(setq no-blinking-cursor t)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)


(provide 'early-init)

;;; early-init.el ends here
