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

;; Minimal UI in init.el when emacs-version < 27
(when (version< emacs-version "27")
  (scroll-bar-mode -1)
  (tool-bar-mode   -1)
  (tooltip-mode    -1)
  (menu-bar-mode   -1))

;; Show buffer name in title
(setq frame-title-format "emacs@%b")

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Startup frame size
(if (eq personal-frame-startup-size 'max)
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

;; Fringe
(fringe-mode '(12 . 12))

;; Installing Fonts
;; https://github.com/domtronn/all-the-icons.el#installing-fonts
(use-package all-the-icons
  :ensure t
  :defer t)

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
  (setq doom-neotree-file-icons t)
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
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-file-name-style 'buffer-name))

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
;; (use-package display-line-numbers
;;   :hook (prog-mode . display-line-numbers-mode))

;; Emacs startup *scratch* buffer
(setq initial-buffer-choice t)

;; ;; https://github.com/cyrus-and/zoom
(use-package zoom
  ;; https://github.com/cyrus-and/zoom/issues/3
  ;; set which-key-popup-type to 'minibuffer to avoid which-key awkward looking
  :ensure t
  ;; :commands (zoom zoom-mode)
  :hook (after-init . zoom-mode))

;; Fonts
;; Source Han Serief: https://github.com/adobe-fonts/source-han-serif
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset (font-spec :family "Source Han Serif"))
;;   (setq face-font-rescale-alist '(("Source Han Serif" . 1.24))))

(defun +ui/frame-config (frame)
  "Custom behaviours for new frames."
  (with-selected-frame frame
    ;; GUI
    (when (display-graphic-p)
      (load-theme (intern personal-doom-theme) t))
    ;; Terminal
    (unless (display-graphic-p)
      (load-theme 'doom-nord t))

    (with-eval-after-load 'dired
      (set-face-foreground 'dired-directory "#3B6EA8"))))

;; Set config now
(+ui/frame-config (selected-frame))

;; Run later, for emacs daemon, emacsclients -c [-nw]
(add-hook 'after-make-frame-functions '+ui/frame-config)

;; Set Fonts
(ignore-errors
  (when (member "SF Mono" (font-family-list))
    ;; Download ans install SF Mono fonts:
    ;; https://github.com/ZulwiyozaPutra/SF-Mono-Font
    (set-frame-font "SF Mono-11.5:weight=semi-bold" nil t))
  (when (member "Symbola" (font-family-list))
    ;; Download specify font for all unicode characters, emoji for example
    ;; http://xahlee.info/comp/unicode_font_download.html
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)))


(provide 'init-ui)

;;; init-ui.el ends here
