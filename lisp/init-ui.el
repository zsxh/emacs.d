;; init-ui.el --- Emacs UI, Startup Screen Settings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs UI, Startup Screen Settings
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(defvar current-theme (if (display-graphic-p)
                          personal-gui-theme
                        personal-tui-theme)
  "Current using theme.")

;; Show buffer name in title
(setq frame-title-format "emacs@%b")

;; Startup frame size
(cond ((eq personal-frame-startup-size 'max)
       (toggle-frame-maximized))
      ((eq personal-frame-startup-size 'fullscreen)
       (toggle-frame-fullscreen)))

;; Fringe
(fringe-mode '(10 . 10))

;; nerd-icons.el works on both GUI and terminal
;; https://github.com/rainstormstudio/nerd-icons.el
;; M-x `nerd-icons-install-fonts'
(use-package nerd-icons
  :defer t)

;; Theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (with-eval-after-load 'org-mode
    (require 'doom-themes-ext-org)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-modal-icon nil
        doom-modeline-hud nil
        doom-modeline-unicode-fallback nil
        doom-modeline-major-mode-icon t
        doom-modeline-window-width-limit 100
        doom-modeline-buffer-file-name-style 'truncate-with-project
        ;; Customize segments
        ;; Requires `eyebrowse-mode' to be enabled or `tab-bar-mode' tabs to be created
        doom-modeline-workspace-name nil))

;; Line Number
(use-package display-line-numbers
  :ensure nil
  :defer t
  ;; :hook (prog-mode . display-line-numbers-mode)
  :config (setq display-line-numbers-type 'relative))

;; Fonts
(ignore-errors
  (let ((font-list (font-family-list)))
    (when (and IS-LINUX (member "SF Mono" (font-family-list)))
      ;; Download ans install SF Mono fonts:
      ;; https://github.com/ZulwiyozaPutra/SF-Mono-Font
      ;; https://github.com/hick/emacs-chinese#emacs-中文基础
      (set-frame-font "SF Mono-13:weight=semi-bold" nil t))
    (when (and IS-MAC (member "Menlo" (font-family-list)))
      (set-frame-font "Menlo-15" nil t))
    (when (member "Symbola" (font-family-list))
      ;; Download specify font for all unicode characters, emoji for example
      ;; http://xahlee.info/comp/unicode_font_download.html
      (set-fontset-font t 'unicode "Symbola" nil 'prepend))))

;; Text Scale
(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-descrease default-text-scale-reset)
  :config
  (default-text-scale-mode 1))

(defun +ui/frame-config (frame)
  "Custom behaviours for new frames."
  (with-selected-frame frame
    (load-theme current-theme t)
    ;; Customize faces
    (+ui/customize-faces)))

(defcustom +ui/customize-faces-fn nil
  "Customize theme faces function."
  :type 'function)

(use-package zsxh-theme-custom
  :ensure nil
  :config
  (setq +ui/customize-faces-fn 'zsxh-fix-theme))

(defun +ui/customize-faces ()
  (when +ui/customize-faces-fn
    (funcall +ui/customize-faces-fn)))

;; Set config now
(with-eval-after-load 'doom-themes
  (+ui/frame-config (selected-frame)))

;; NOTE: It's usually best to put code that's GUI/Terminal (display-graphic-p) specific in `after-make-frame-functions'
;; Run later, for emacs daemon, emacsclients -c [-nw]
(add-hook 'after-make-frame-functions '+ui/frame-config)

(defun load-theme-a (orig-fn &rest args)
  ;; TODO: reset faces in `+ui/customize-faces'
  ;; get rid of all loaded themes
  (mapcar #'disable-theme custom-enabled-themes)
  (setq current-theme (cl-first args))
  (apply orig-fn args)
  (+ui/customize-faces))

(advice-add 'load-theme :around 'load-theme-a)


(provide 'init-ui)

;;; init-ui.el ends here
