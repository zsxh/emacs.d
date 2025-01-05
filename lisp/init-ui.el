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

;; When Context Menu mode is enabled, clicking the mouse button down-mouse-3
;; activates the menu whose contents depends on its surrounding context
(context-menu-mode 1)

;; Startup frame size
(cond ((eq personal-frame-startup-size 'max)
       (toggle-frame-maximized))
      ((eq personal-frame-startup-size 'fullscreen)
       (toggle-frame-fullscreen)))

;; Fringe
;; (fringe-mode '(10 . 10))

;; nerd-icons.el works on both GUI and terminal
;; https://github.com/rainstormstudio/nerd-icons.el
;; M-x `nerd-icons-install-fonts'
(use-package nerd-icons
  :defer t)

;;  Use nerd-icons for archive-mode and tar-mode
(use-package nerd-icons-archive
  :vc (:url "https://github.com/abougouffa/nerd-icons-archive")
  :hook (after-init . nerd-icons-archive-mode))

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
  (setq evil-normal-state-tag "<N>"
        evil-insert-state-tag "<I>"
        evil-visual-state-tag "<V>"
        evil-operator-state-tag "<O>"
        evil-replace-state-tag "<R>"
        evil-motion-state-tag "<M>"
        evil-emacs-state-tag "<E>")
  (setq doom-modeline-modal-icon nil
        doom-modeline-modal-modern-icon nil
        doom-modeline-hud nil
        doom-modeline-unicode-fallback nil
        doom-modeline-major-mode-icon t
        doom-modeline-window-width-limit 100
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-workspace-name nil))

;; Line Number
(use-package display-line-numbers
  :ensure nil
  :defer t
  ;; :hook (prog-mode . display-line-numbers-mode)
  :config (setq display-line-numbers-type 'relative))

;; Fonts
;; Download ans install SF Mono fonts for Linux
;; https://github.com/ZulwiyozaPutra/SF-Mono-Font
;; https://github.com/hick/emacs-chinese#emacs-中文基础
(defun +ui/adjust-font-size ()
  "Adjust FRAME font size base on `frame-monitor-attributes'"
  (let* ((geometry (frame-monitor-attribute 'geometry))
         (mm-size (frame-monitor-attribute 'mm-size))
         (width-px (nth 2 geometry)) ; Pixel
         (width-mm (nth 2 mm-size)) ; Millimeter
         (font-name (or
                     ;; custom font
                     (and IS-LINUX (member "SF Mono" (font-family-list)) "SF Mono")
                     ;; default font
                     (car
                      (string-split
                       (aref
                        (font-info
                         (face-attribute 'default :font))
                        1)
                       ":"))))
         (size (cond ((>= width-px 2560) 17)
                     ((>= width-px 1512) 15)
                     (t 13)))
         (font (cond ((string-equal "SF Mono" font-name) (format "SF Mono-%d:weight=semi-bold" size))
                     (t (format "%s-%d" font-name size)))))
    (set-frame-font font)))

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
    (custom-theme-set-faces
     theme
     `(dired-directory ((t :foreground ,(doom-color 'blue))))
     `(nerd-icons-completion-dir-face ((t :foreground ,(doom-color 'blue))))
     '(magit-diff-revision-summary ((t :inherit magit-diff-hunk-heading-highlight)))
     '(eldoc-highlight-function-argument ((t :inherit font-lock-variable-name-face
                                             :weight bold)))
     '(Info-quoted ((t :inherit (font-lock-constant-face fixed-pitch-serif))))
     `(orderless-match-face-0 ((t :weight bold :foreground ,(doom-color 'blue))))
     `(orderless-match-face-1 ((t :weight bold :foreground ,(doom-color 'magenta))))
     `(orderless-match-face-2 ((t :weight bold :foreground ,(doom-color 'green))))
     `(orderless-match-face-3 ((t :weight bold :foreground ,(doom-color 'yellow))))
     ;; https://github.com/akermu/emacs-libvterm/issues/58#issuecomment-516950648
     `(vterm-color-black ((t :background ,(doom-color 'base6))))
     `(corfu-current ((t :background ,(doom-color 'region)
                         :foreground ,(doom-color 'fg))))
     `(tab-bar-tab ((t :foreground ,(doom-color 'highlight)
                       :background ,(doom-color 'bg)
                       :weight bold)))
     `(show-paren-match ((t :foreground ,(doom-color 'red)
                            :background ,(doom-color 'base0)
                            :weight ultra-bold
                            :underline t)))
     '(pulse-highlight-start-face ((t :background "#51afef" :extend t))))))

(advice-add 'load-theme :before #'+ui/disable-previous-theme)
(advice-add 'load-theme :after #'+ui/custom-theme-faces)

(defun +ui/frame-config (frame)
  "Custom behaviours for new frames."
  (with-selected-frame frame
    (ignore-errors (load-theme current-theme t))
    (ignore-errors (+ui/adjust-font-size))))

;; Set config now
(with-eval-after-load 'doom-themes
  (+ui/frame-config (selected-frame)))

;; NOTE: It's usually best to put code that's GUI/Terminal (display-graphic-p) specific in `after-make-frame-functions'
;; Run later, for emacs daemon, emacsclients -c [-nw]
(add-hook 'after-make-frame-functions '+ui/frame-config)


(provide 'init-ui)

;;; init-ui.el ends here
