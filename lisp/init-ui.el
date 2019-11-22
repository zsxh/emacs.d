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

;; Show buffer name in title
(setq frame-title-format "emacs@%b")

;; Startup frame size

(cond ((eq personal-frame-startup-size 'max)
       (toggle-frame-maximized))
      ((eq personal-frame-startup-size 'fullscreen)
       (toggle-frame-fullscreen)))

;; Fringe
(fringe-mode '(12 . 12))

;; Installing Fonts
;; https://github.com/domtronn/all-the-icons.el#installing-fonts
(use-package all-the-icons
  :ensure t
  :defer t
  :config
  ;; If you experience a slow down in performace when rendering multiple icons simultaneously,
  ;; you can try setting the following variable. If non-nil, don't compact font caches during GC.
  (setq inhibit-compacting-font-caches t))

;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (setq doom-themes-neotree-file-icons t)
  (doom-themes-neotree-config) ; all-the-icons fonts must be installed!

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :defer t
  :preface
  (defun my-doomline-init ()
    (doom-modeline-init)
    (with-current-buffer "*Messages*"
      (doom-modeline-set-modeline 'main)))
  :hook (after-init . my-doomline-init)
  :config
  (setq doom-modeline-evil-state-icon nil
        doom-modeline-unicode-fallback nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'buffer-name))

;; ;; Display time on modeline
(setq display-time-format "%Y/%m/%d %A %H:%M")
(setq display-time-default-load-average nil) ; don't show load avera
(display-time-mode)

(defcustom +ui/display-time-format-style 'long
  "Customize time format."
  :type '(radio (const :tag "Display time format, Year/Month/Day Weekname Hour/Minute" long)
                (const :tag "Display time format, Hour/Minute" short)))

(defun +ui/toggle-display-time-mode ()
  "Display time format depending on window-width."
  (unless (active-minibuffer-window)
    (cond
     ((and (eq +ui/display-time-format-style 'long)
           (<= (window-width) 110))
      (setq +ui/display-time-format-style 'short)
      (setq display-time-format "%H:%M")
      (display-time-mode -1)
      (display-time-mode 1))
     ((and (eq +ui/display-time-format-style 'short)
           (> (window-width) 110))
      (setq +ui/display-time-format-style 'long)
      (setq display-time-format "%Y/%m/%d %A %H:%M")
      (display-time-mode -1)
      (display-time-mode 1)))))

(add-hook 'window-configuration-change-hook '+ui/toggle-display-time-mode)

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

;; Set Fonts
(ignore-errors
  ;; Download ans install SF Mono fonts:
  ;; https://github.com/ZulwiyozaPutra/SF-Mono-Font
  (if (member "SF Mono" (font-family-list))
      (set-frame-font "SF Mono-11.5:weight=semi-bold" nil t)
    (message "[WARN] font \"SF Mono\" not found"))
  ;; Download specify font for all unicode characters, emoji for example
  ;; http://xahlee.info/comp/unicode_font_download.html
  (if (member "Symbola" (font-family-list))
      (set-fontset-font t 'unicode "Symbola" nil 'prepend)
    (message "[WARN] font \"Symbola\" not found")))

(defun +ui/frame-config (frame)
  "Custom behaviours for new frames."
  (with-selected-frame frame
    (load-theme current-theme t)
    ;; Customize faces
    (+ui/customize-faces)))

(defun +ui/customize-faces ()
  (pcase current-theme
    ('doom-nord-light
     (progn
       (with-eval-after-load 'dired
         (set-face-foreground 'dired-directory "#3B6EA8"))
       (with-eval-after-load 'all-the-icons-dired
         (set-face-foreground 'all-the-icons-dired-dir-face "#3B6EA8"))
       (with-eval-after-load 'markdown-mode
         (set-face-background 'markdown-code-face "#E0E0E0"))
       (with-eval-after-load 'org
         ;; Org block face
         (set-face-background 'org-block "#E0E0E0")
         ;; https://www.reddit.com/r/emacs/comments/diahh1/emacs_27_update_changed_how_highlighted_lines/
         ;; The new face attribute ':extend' controls whether to use the face for displaying the empty space beyond end of line (EOL) till the edge of the window.
         (when (>= emacs-major-version 27)
           (set-face-extend 'org-block t))
         (set-face-background 'org-quote nil)
         (set-face-background 'org-block-begin-line nil)
         (set-face-background 'org-block-end-line nil))
       (with-eval-after-load 'mmm-vars
         (set-face-background 'mmm-default-submode-face "#E5E5E5"))))
    ('doom-one
     (progn
       (with-eval-after-load 'all-the-icons-dired
         (set-face-foreground 'all-the-icons-dired-dir-face "#b478dd"))
       (with-eval-after-load 'org
         ;; Org block face
         ;; https://www.reddit.com/r/emacs/comments/diahh1/emacs_27_update_changed_how_highlighted_lines/
         ;; The new face attribute ':extend' controls whether to use the face for displaying the empty space beyond end of line (EOL) till the edge of the window.
         (when (>= emacs-major-version 27)
           (set-face-extend 'org-block t))
         (set-face-background 'org-quote nil)
         (set-face-background 'org-block-begin-line nil)
         (set-face-background 'org-block-end-line nil))
       ))))

;; Set config now
(+ui/frame-config (selected-frame))

;; Run later, for emacs daemon, emacsclients -c [-nw]
(add-hook 'after-make-frame-functions '+ui/frame-config)


(provide 'init-ui)

;;; init-ui.el ends here
