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

;; Minimal UI in init.el when emacs-version < 27
(when (version< emacs-version "27")
  (scroll-bar-mode -1)
  (tool-bar-mode   -1)
  (tooltip-mode    -1)
  (menu-bar-mode   -1))

;; Disable cursor blinking
;; FIXME: emacs 28.1 no-blinking-cursor
(when (and no-blinking-cursor
           blink-cursor-mode)
  (blink-cursor-mode -1))

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
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-modal-icon nil
        doom-modeline-unicode-fallback nil
        doom-modeline-major-mode-icon t
        doom-modeline-window-width-limit 100
        doom-modeline-buffer-file-name-style 'auto)

  (defun display-battery-if-offline (fn)
    (let* ((data (and (bound-and-true-p display-battery-mode)
                      (funcall battery-status-function)))
           (status (cdr (assoc ?L data)))
           (online-p (if (version< emacs-version "28")
                         (string-equal "AC" status)
                       (string-equal "on-line" status))))
      (if online-p
          (setq doom-modeline--battery-status nil)
        (funcall fn))))

  (advice-add 'doom-modeline-update-battery-status :around 'display-battery-if-offline)

  ;; customize display time window width,
  (doom-modeline-def-segment misc-info
    "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
    (when (and (doom-modeline--active)
               (>= (window-width) 90))
      '("" mode-line-misc-info))))

;; Display time on modeline
(defvar +ui/time-format-short "[%H:%M]"
  "Short display time format.")

(defvar +ui/time-format-long "[%Y-%m-%d %a %H:%M]"
  "Long display time format.")

(defcustom +ui/display-time-format-style 'long
  "Customize time format."
  :type '(radio (const :tag "Display time format, Year/Month/Day/Weekname/Hour:Minute" long)
                (const :tag "Display time format, Hour/Minute" short)))

(setq display-time-format (if (eq 'long +ui/display-time-format-style)
                              +ui/time-format-long
                            +ui/time-format-short))
(setq display-time-default-load-average nil) ; don't show load avera
(add-hook 'after-init-hook 'display-time-mode)

(defun +ui/toggle-display-time-mode ()
  "Display time format depending on window-width."
  (unless (active-minibuffer-window)
    (cond
     ((and (eq +ui/display-time-format-style 'long)
           (<= (window-width) 110))
      (setq +ui/display-time-format-style 'short)
      (setq display-time-format +ui/time-format-short)
      (display-time-mode -1)
      (display-time-mode 1))
     ((and (eq +ui/display-time-format-style 'short)
           (> (window-width) 110))
      (setq +ui/display-time-format-style 'long)
      (setq display-time-format +ui/time-format-long)
      (display-time-mode -1)
      (display-time-mode 1)))))

(add-hook 'window-configuration-change-hook '+ui/toggle-display-time-mode)

;; Display battery status
(add-hook 'after-init-hook 'display-battery-mode)

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
;; FIXME: company-box-background is not extended after text scale resizing
(use-package default-text-scale
  :commands (default-text-scale-increase default-text-scale-descrease default-text-scale-reset)
  :config
  (default-text-scale-mode 1))

;; Set Fonts
(ignore-errors
  ;; Download ans install SF Mono fonts:
  ;; https://github.com/ZulwiyozaPutra/SF-Mono-Font
  (if (member "SF Mono" (font-family-list))
      (set-frame-font "SF Mono-12:weight=semi-bold" nil t)
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
  (setq current-theme (first args))
  (apply orig-fn args)
  (+ui/customize-faces))

(advice-add 'load-theme :around 'load-theme-a)


(provide 'init-ui)

;;; init-ui.el ends here
