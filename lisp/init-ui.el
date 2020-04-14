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
  :defer t
  :config
  ;; If you experience a slow down in performace when rendering multiple icons simultaneously,
  ;; you can try setting the following variable. If non-nil, don't compact font caches during GC.
  (setq inhibit-compacting-font-caches t))

;; Theme
(use-package doom-themes
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
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-modal-icon nil
        doom-modeline-unicode-fallback nil
        doom-modeline-major-mode-icon t
        doom-modeline-window-width-limit 110
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

  (advice-add 'doom-modeline-update-battery-status :around 'display-battery-if-offline))

(use-package mini-frame
  :if (display-graphic-p)
  ;; :hook (after-init . mini-frame-mode)
  :defer t
  :config
  (setq mini-frame-ignore-commands '(evil-ex
                                     swiper
                                     dired-narrow
                                     eval-expression
                                     eaf-proxy-insert_or_open_link
                                     eaf-proxy-insert_or_open_link_new_buffer))
  (setq mini-frame-color-shift-step 15)
  (setq mini-frame-show-parameters
        '((top . 0.3)
          (width . 0.7)
          (left . 0.5)
          (min-height . 2))))

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
         (set-face-background 'org-quote nil)
         (set-face-background 'org-block-begin-line nil)
         (set-face-background 'org-block-end-line nil))
       (with-eval-after-load 'mmm-vars
         (set-face-background 'mmm-default-submode-face "#E5E5E5"))
       (with-eval-after-load 'jupyter-repl
         (set-face-foreground 'jupyter-repl-input-prompt "#4F894C")
         (set-face-background 'jupyter-repl-traceback "#FBF8EF"))))
    ('doom-one
     (progn
       (with-eval-after-load 'dired
         (set-face-foreground 'dired-directory "#51afef"))
       (with-eval-after-load 'all-the-icons-dired
         (set-face-foreground 'all-the-icons-dired-dir-face "#3B6EA8"))
       (with-eval-after-load 'org
         (set-face-background 'org-quote nil)
         (set-face-background 'org-block-begin-line nil)
         (set-face-background 'org-block-end-line nil))
       (with-eval-after-load 'ein-cell
         (set-face-attribute 'ein:cell-input-area nil :background "#22262e")
         (set-face-attribute 'ein:cell-input-prompt nil :foreground "#4F894C" :background "#282c34")
         (set-face-attribute 'ein:cell-output-prompt nil :foreground "darkred" :background "#282c34"))
       (with-eval-after-load 'jupyter-repl
         (set-face-foreground 'jupyter-repl-input-prompt "#4F894C")
         (set-face-background 'jupyter-repl-traceback "#4B483F"))
       (with-eval-after-load 'ivy
         (let ((base3 (doom-color 'base3))
               (blue (doom-color 'blue))
               (magenta (doom-color 'magenta))
               (green (doom-color 'green))
               (yellow (doom-color 'yellow))
               (violet (doom-color 'violet)))
           (set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground yellow :background base3 :weight 'bold)
           (set-face-attribute 'ivy-minibuffer-match-face-2 nil :foreground magenta :background base3 :weight 'bold)
           (set-face-attribute 'ivy-minibuffer-match-face-3 nil :foreground green :background base3 :weight 'bold)
           (set-face-attribute 'ivy-minibuffer-match-face-4 nil :foreground blue :background base3 :weight 'bold)
           (set-face-attribute 'ivy-minibuffer-match-highlight nil :foreground violet :weight 'bold)
           (with-eval-after-load 'swiper
             (set-face-attribute 'swiper-match-face-1 nil :inherit 'ivy-minibuffer-match-face-1)
             (set-face-attribute 'swiper-match-face-2 nil :inherit 'ivy-minibuffer-match-face-2)
             (set-face-attribute 'swiper-match-face-3 nil :inherit 'ivy-minibuffer-match-face-3)
             (set-face-attribute 'swiper-match-face-4 nil :inherit 'ivy-minibuffer-match-face-4))))))
    ('doom-solarized-light
     (progn
       (with-eval-after-load 'dired
         (set-face-foreground 'dired-directory "#268bd2"))
       (with-eval-after-load 'paren
         (set-face-background 'show-paren-match "#E5E5E5"))))
    ('doom-dark+
     (progn
       (set-face-background 'fringe (doom-color 'bg))
       (with-eval-after-load 'company-posframe
         (set-face-background 'company-posframe-active-backend-name (doom-color 'modeline-bg))
         (set-face-background 'company-posframe-inactive-backend-name (doom-color 'modeline-bg-alt)))
       (with-eval-after-load 'paren
         (set-face-foreground 'show-paren-match "grep")))))

  ;; https://www.reddit.com/r/emacs/comments/diahh1/emacs_27_update_changed_how_highlighted_lines/
  ;; The new face attribute ':extend' controls whether to use the face for displaying the empty space beyond end of line (EOL) till the edge of the window.
  (when (fboundp 'set-face-extend)
    (with-eval-after-load 'org
      (set-face-extend 'org-block t)
      (set-face-extend 'org-block-begin-line t)
      (set-face-extend 'org-block-end-line t))
    (with-eval-after-load 'ein-cell
      (set-face-extend 'ein:cell-input-area t))
    (with-eval-after-load 'jupyter-repl
      (set-face-extend 'jupyter-repl-traceback t))
    (with-eval-after-load 'company-box
      (set-face-extend 'company-box-selection t)))

  ;; global settings
  (with-eval-after-load 'magit-diff
    (set-face-attribute 'magit-diff-revision-summary nil :inherit 'magit-diff-hunk-heading-highlight)))

;; Set config now
(+ui/frame-config (selected-frame))

;; Run later, for emacs daemon, emacsclients -c [-nw]
(add-hook 'after-make-frame-functions '+ui/frame-config)


(provide 'init-ui)

;;; init-ui.el ends here
