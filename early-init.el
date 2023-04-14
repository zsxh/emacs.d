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

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. We handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Quickstart: precompute activation actions for faster start up
;; However, if you do this, then you have to manually run the command ‘package-quickstart-refresh’ when the
;; activations need to be changed, such as when you change the value of  ‘package-load-list’
;; (setq package-quickstart t)

;; Emacs startup *scratch* buffer
(setq inhibit-startup-screen t
      initial-buffer-choice  t)

;; Disable cursor blinking
(setq no-blinking-cursor t
      visible-cursor nil)
(blink-cursor-mode -1)

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

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Inhibit automatic native compilation of loaded .elc files
;; (setq native-comp-deferred-compilation nil
;;       inhibit-automatic-native-compilation t)

;; If non-nil and there was input pending at the beginning of the command,
;; the `fontification_functions` hook is not run.  This usually does not
;; affect the display because redisplay is completely skipped anyway if input
;; was pending, but it can make scrolling smoother by avoiding
;; unnecessary fontification.
;; It is similar to `fast-but-imprecise-scrolling' with similar tradeoffs,
;; but with the advantage that it should only affect the behavior when Emacs
;; has trouble keeping up with the incoming input rate.
(setq redisplay-skip-fontification-on-input t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)

;; no wait for X events
(setq x-wait-for-event-timeout 0)


(provide 'early-init)

;;; early-init.el ends here
