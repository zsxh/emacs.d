;; init-eaf.el --- Emacs Application Framework	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Emacs Application Framework
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; https://github.com/manateelazycat/emacs-application-framework#install
;; Install in pyenv enviroment
;; $ pip install PyQt5 PyQtWebEngine dbus-python PyMuPDF
;; $ sudo pacman -S wmctrl
(use-package eaf
  :load-path "~/.emacs.d/submodules/emacs-application-framework"
  :commands (eaf-open
             eaf-open-browser
             eaf-open-browser-with-history
             eaf-open-dash
             eaf-open-bookmark
             eaf-open-this-from-dired
             eaf-open-office
             eaf-open-mindmap
             eaf-create-mindmap
             eaf-get-file-name-extension)
  :hook (eaf-mode . (lambda () (setq left-fringe-width 0
                                     right-fringe-width 0)))
  :init
  (setq eaf-app-extensions-alist
        '(("pdf-viewer" . eaf-pdf-extension-list)
          ("image-viewer" . eaf-image-extension-list)
          ("video-player" . eaf-video-extension-list)
          ("mindmap" . eaf-mindmap-extension-list)))
  (progn
    (defun eaf-find-file (orig-fn &rest args)
      (let* ((file (car args))
             (file-extension (file-name-extension file))
             (ext (if file-extension (downcase file-extension) nil)))
        (cond
         ((not ext) (apply orig-fn args))
         ((member (eaf-get-file-name-extension file) eaf-office-extension-list)
          (eaf-open-office file))
         ((eaf--get-app-for-extension
           (eaf-get-file-name-extension file))
          (eaf-open file))
         (t (apply orig-fn args)))))
    (advice-add #'find-file :around #'eaf-find-file)
    (with-eval-after-load 'org
      (setq browse-url-browser-function 'eaf-open-browser)))
  :config
  (setq
   eaf-python-command (expand-file-name "~/.pyenv/versions/3.8.6/bin/python")
   eaf-enable-debug t
   eaf-browser-default-search-engine "duckduckgo"
   eaf-config-location (expand-file-name (locate-user-emacs-file ".cache/eaf/")))

  (eaf-setq eaf-browser-blank-page-url "https://duckduckgo.com")
  (eaf-setq eaf-browser-default-zoom "1.2")
  (eaf-setq eaf-browser-remember-history "true")
  (eaf-setq eaf-camera-save-path "~/Download")
  (eaf-setq eaf-browser-download-path "~/Download")
  (eaf-setq eaf-mindmap-save-path "~/Download")
  (eaf-setq eaf-browser-dark-mode "false")
  (eaf-setq eaf-pdf-dark-mode "false")

  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-u" eaf-pdf-viewer-keybinding)

  (eaf-bind-key +eaf/switch-to-eww "C-t" eaf-browser-keybinding)
  (eaf-bind-key nil "M-u" eaf-browser-keybinding)
  (eaf-bind-key clear_focus "M-p" eaf-browser-keybinding)
  (eaf-bind-key nil "T" eaf-browser-keybinding)
  (eaf-bind-key recover_prev_close_page "X" eaf-browser-keybinding)

  (evil-define-key 'normal eaf-pdf-outline-mode-map (kbd "RET") 'eaf-pdf-outline-jump)

  (require 'dash)
  (when personal-eaf-grip-token
    (setq eaf-grip-token personal-eaf-grip-token))

  (setq eaf-proxy-host personal-proxy-http-host
        eaf-proxy-port (format "%s" personal-proxy-http-port)
        eaf-proxy-type "http")

  ;; override
  (defun eaf-open-dev-tool-page ()
    (split-window (selected-window)
                  (/ (* (nth 3 (eaf-get-window-allocation (selected-window))) 2) 3)
                  nil t)
    (other-window 1)
    (eaf-open "about:blank" "browser" "dev_tools"))

  ;; hack eaf for evil-mode
  (add-hook 'eaf-mode-hook
            (lambda ()
              (setq-local evil-motion-state-map nil)
              ;; "C-z" normal-state -> emacs-state
              ;; ":" evil-ex
              (setq-local evil-normal-state-map
                          '(keymap
                            (58 . evil-ex)
                            (26 . evil-emacs-state)))))

  (add-hook 'eaf-browser-hook
            (lambda ()
              ;; browser toggle insert/normal state except in devtool
              ;; devtool buffer will first open about:blank page and then redirect to devltools:// path
              (unless (string-prefix-p "about:blank" eaf--buffer-url)
                (evil-local-set-key 'insert (kbd "<escape>") 'eaf-proxy-clear_focus)
                (add-hook 'post-command-hook 'eaf-is-focus-toggle nil t))))

  (defun eaf-is-focus-toggle ()
    "Toggle is-focus behavior in eaf-mode buffers."
    (if (eaf-call "call_function" eaf--buffer-id "is_focus")
        (unless (evil-insert-state-p)
          (evil-insert-state))
      (when (evil-insert-state-p)
        (evil-normal-state))))

  (defun eaf-devtool-insert-advice (orig-fn &rest args)
    (if (and (string-prefix-p "devtools://" eaf--buffer-url)
             (not (evil-insert-state-p)))
        (evil-insert-state)
      (apply orig-fn args)))

  (advice-add 'eaf-proxy-insert_or_focus_input :around 'eaf-devtool-insert-advice)

  ;; utils
  (defun +eaf/switch-to-eww ()
    (interactive)
    (let* ((url (eaf-get-path-or-url))
           (eww-buffer (car (-filter (lambda (buffer)
                                       (with-current-buffer buffer
                                         (and (derived-mode-p 'eww-mode)
                                              (equal url (plist-get eww-data :url)))))
                                     (buffer-list)))))
      (if eww-buffer
          (switch-to-buffer eww-buffer)
        (eww url))))

  (defun eaf-open-dash (url &optional args)
    (interactive)
    (eaf-open url "browser" nil))

  (defvar +eaf/browser-current-theme (cdr (assoc 'eaf-browser-dark-mode eaf-var-list)))

  ;; TODO: keybindings for eaf-mode
  (defun +eaf/cycle-browser-theme ()
    (interactive)
    (if (derived-mode-p 'eaf-mode)
        (let ((next-theme (cond
                           ((string= +eaf/browser-current-theme "follow") "false")
                           ((string= +eaf/browser-current-theme "false") "true")
                           ((string= +eaf/browser-current-theme "true") "follow"))))
          (message "switch to dark-mode=%s" next-theme)
          (setq +eaf/browser-current-theme next-theme)
          (eaf-setq eaf-browser-dark-mode +eaf/browser-current-theme)
          (eaf-proxy-refresh_page))
      (message "+eaf/cycle-browser-theme can only be called in an EAF buffer"))))


(provide 'init-eaf)

;;; init-eaf.el ends here
