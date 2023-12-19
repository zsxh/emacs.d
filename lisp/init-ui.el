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

;; Custom Faces
(defun +ui/disable-previous-theme (theme &optional _ _)
  (mapc #'disable-theme custom-enabled-themes)
  (setq current-theme theme))

(defun +ui/custom-theme-faces (theme &optional _ _)
  "Customize THEME faces."
  (let ((custom--inhibit-theme-enable nil))
    (apply 'custom-theme-set-faces theme
           ;; Global faces
           '(magit-diff-revision-summary ((t :inherit 'magit-diff-hunk-heading-highlight)))
           ;; Per Theme faces
           (pcase theme
             ('doom-one-light
              '((dired-directory ((t (:foreground "#3B6EA8"))))
                (nerd-icons-completion-dir-face ((t (:foreground "#3B6EA8"))))))
             ('doom-nord-light
              '((dired-directory ((t (:foreground "#3B6EA8"))))
                (nerd-icons-completion-dir-face ((t (:foreground "#3B6EA8"))))))
             ('doom-one
              '((dired-directory ((t (:foreground "#51afef"))))
                (nerd-icons-completion-dir-face ((t (:foreground "#51afef"))))))
             ('doom-one
              '((dired-directory ((t (:foreground "#51afef"))))
                (nerd-icons-completion-dir-face ((t (:foreground "#51afef"))))))
             ('doom-solarized-light
              '((dired-directory ((t (:foreground "#268bd2"))))
                (nerd-icons-completion-dir-face ((t (:foreground "#268bd2"))))
                (show-paren-match ((t (:background "#E5E5E5"))))))
             ('doom-dark+
              `((fringe ((t (:background ,(face-attribute 'default :background)))))
                (nerd-icons-completion-dir-face ((t (:foreground "#E5E5E5"))))
                (company-posframe-active-backend-name ((t (:background ,(doom-color 'modeline-bg)))))
                (company-posframe-inactive-backend-name ((t (:background ,(doom-color 'modeline-bg-alt)))))
                (show-paren-match ((t (:background "#4e4e4e"))))))))))

(advice-add 'load-theme :before #'+ui/disable-previous-theme)
(advice-add 'load-theme :after #'+ui/custom-theme-faces)

(defun +ui/frame-config (frame)
  "Custom behaviours for new frames."
  (with-selected-frame frame
    (load-theme current-theme t)))

;; Set config now
(with-eval-after-load 'doom-themes
  (+ui/frame-config (selected-frame)))

;; NOTE: It's usually best to put code that's GUI/Terminal (display-graphic-p) specific in `after-make-frame-functions'
;; Run later, for emacs daemon, emacsclients -c [-nw]
(add-hook 'after-make-frame-functions '+ui/frame-config)


(provide 'init-ui)

;;; init-ui.el ends here
