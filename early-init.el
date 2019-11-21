;; early-init.el --- early initial	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
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

;; Minimal UI in early-init.el when emacs-version > 27.
;; This will Speedup ui loading for 300ms
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Disable cursor blinking
(setq no-blinking-cursor t)

;; Emacs startup *scratch* buffer
(setq inhibit-startup-screen t
      initial-buffer-choice  nil)


(provide 'early-init)

;;; early-init.el ends here
