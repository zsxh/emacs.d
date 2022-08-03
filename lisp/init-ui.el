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

;; TODO: It's usually best to put code that;s GUI/Terminal specific in `after-make-frame-functions'
;; (add-hook 'after-make-frame-functions
;;           (lambda ()
;;             (if (display-graphic-p)
;;                 ;; we do something only in GUI Emacs
;;                 nil
;;               ;; we do something only in terminal Emacs
;;               nil)))

(defvar current-theme (if (display-graphic-p)
                          personal-gui-theme
                        personal-tui-theme)
  "Current using theme.")

;; Minimal UI in init.el when emacs-version < 27
(when (version< emacs-version "27")
  (scroll-bar-mode -1)
  (tool-bar-mode   -1)
  (tooltip-mode    -1)
  (menu-bar-mode   -1))

;; Show buffer name in title
(setq frame-title-format "emacs@%b")

;; Startup frame size

(cond ((eq personal-frame-startup-size 'max)
       (toggle-frame-maximized))
      ((eq personal-frame-startup-size 'fullscreen)
       (toggle-frame-fullscreen)))

;; Fringe
(fringe-mode '(10 . 10))

;; Installing Fonts
;; https://github.com/domtronn/all-the-icons.el#installing-fonts
(use-package all-the-icons
  :defer t)

;; https://github.com/seagle0128/icons-in-terminal.el
;; M-x: icons-in-terminal-install-font
(use-package icons-in-terminal
  :defer t
  :load-path "~/.emacs.d/submodules/icons-int-terminal")

;; Theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable neotree theme
  (setq doom-themes-neotree-file-icons t)
  (doom-themes-neotree-config) ; all-the-icons fonts must be installed!
  ;; Enable treemacs theme
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
  :defer t
  ;; :hook (prog-mode . display-line-numbers-mode)
  :config (setq display-line-numbers-type 'relative))

;; Fonts
;; Source Han Serief: https://github.com/adobe-fonts/source-han-serif
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset (font-spec :family "Source Han Serif"))
;;   (setq face-font-rescale-alist '(("Source Han Serif" . 1.24))))

;; Text Scale
(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-descrease default-text-scale-reset)
  :config
  (default-text-scale-mode 1))

;; Set Fonts
(ignore-errors
  ;; Download ans install SF Mono fonts:
  ;; https://github.com/ZulwiyozaPutra/SF-Mono-Font
  ;; https://github.com/hick/emacs-chinese#emacs-中文基础
  (if (member "SF Mono" (font-family-list))
      (set-frame-font "SF Mono-13:weight=semi-bold" nil t)
    (message "[WARN] font \"SF Mono\" not found"))
  ;; Download specify font for all unicode characters, emoji for example
  ;; http://xahlee.info/comp/unicode_font_download.html
  (if (member "Symbola" (font-family-list))
      (set-fontset-font t 'unicode "Symbola" nil 'prepend)
    (message "[WARN] font \"Symbola\" not found")))

;; TODO: Typographic Ligatures in Emacs, https://github.com/mickeynp/ligature.el

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
(+ui/frame-config (selected-frame))

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
