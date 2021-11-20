;; init-custom.el --- Customizations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Customizations
;;

;;; Code:

(defgroup personal nil
  "Personal Emacs customizations."
  :group 'convenience)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(if (file-exists-p custom-file)
    (load custom-file))

(defcustom personal-package-archives 'melpa
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Melpa Mirror" melpa-mirror)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tencent" tencent)
          (const :tag "Tuna" tuna)))

(defcustom personal-eaf-grip-token nil
  "Github personal access token for eaf-markdown-previewer.
https://github.com/manateelazycat/emacs-application-framework#markdown-previewer"
  :type 'string)

(defcustom personal-gui-theme-day 'doom-solarized-light
  "Customize GUI with doom-themes, \"doom-one\", \"doom-nord-light\" for example.
Check https://github.com/hlissner/emacs-doom-themes"
  :type 'symbol)

(defcustom personal-gui-theme-night 'doom-solarized-dark
  "Customize GUI with doom-themes, \"doom-one\", \"doom-nord-light\" for example.
Check https://github.com/hlissner/emacs-doom-themes"
  :type 'symbol)

(defun +custom/rand-theme (themes)
  (nth (random (length themes)) themes))

(defcustom personal-gui-theme-random-p nil
  "Random pick GUI theme."
  :type 'boolean)

(defcustom personal-light-gui-themes '(doom-one-light doom-solarized-light)
  "Random light themes"
  :type '(repeat symbol))

(defcustom personal-dark-gui-themes '(doom-one doom-dark+)
  "Random dark themes"
  :type '(repeat symbol))

(defcustom personal-gui-theme (let* ((hour (nth 2 (decode-time (current-time))))
                                     (day? (member hour (number-sequence 7 17)))
                                     (theme-0 (if day? personal-gui-theme-day personal-gui-theme-night))
                                     (theme-1 (and personal-gui-theme-random-p
                                                   (+custom/rand-theme
                                                    (cond (day? personal-light-gui-themes)
                                                          (t personal-dark-gui-themes))))))
                                (or theme-1 theme-0))
  "Customize Terminal UI with doom-themes, \"doom-one\", \"doom-nord-light\" for example.
Check https://github.com/hlissner/emacs-doom-themes")

(defcustom personal-tui-theme personal-gui-theme-night
  "Customize Terminal UI with doom-themes, \"doom-one\", \"doom-nord-light\" for example.
Check https://github.com/hlissner/emacs-doom-themes"
  :type 'symbol)

(defcustom personal-frame-startup-size 'max
  "Startup frame size. `'max' means maximized frame and `'fullscreen' means fullscreen frame."
  :type '(choice
          (const :tag "Max" max)
          (const :tag "Fullscreen" fullscreen)))

(defcustom personal-shell-executable "/usr/bin/zsh"
  "Shell used in `term' and `ansi-term'."
  :type 'string)

(defcustom personal-proxy-http-host "127.0.0.1"
  "Set http(s) proxy host."
  :type 'string)

(defcustom personal-proxy-http-port 1081
  "Set http(s) proxy port."
  :type 'integer)

(defcustom personal-proxy-socks5-port 1080
  "Set socks5 proxy port."
  :type 'integer)

(provide 'init-custom)

;;; init-custom.el ends here
