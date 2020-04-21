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

;; Install EAF
;; https://github.com/manateelazycat/emacs-application-framework#install
(use-package eaf
  :load-path "~/.emacs.d/submodules/emacs-application-framework"
  :commands (eaf-open
             eaf-open-browser
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
  (setq eaf-python-command "/usr/bin/python" ; Install dependencies from arch repo
        eaf-enable-debug nil
        eaf-browser-default-search-engine "duckduckgo"
        eaf-config-location (expand-file-name (locate-user-emacs-file ".cache/eaf")))

  (eaf-setq eaf-browser-blank-page-url "https://duckduckgo.com")
  (eaf-setq eaf-browser-default-zoom "1.2")
  (eaf-setq eaf-browser-remember-history "true")
  (eaf-setq eaf-camera-save-path "~/Download")
  (eaf-setq eaf-browser-download-path "~/Download")
  (eaf-setq eaf-mindmap-save-path "~/Download")

  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-u" eaf-pdf-viewer-keybinding)

  (eaf-bind-key eaf-switch-to-eww "C-t" eaf-browser-keybinding)
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
    ;; (delete-other-windows)
    (split-window (selected-window) (/ (* (nth 3 (eaf-get-window-allocation (selected-window))) 2) 3) nil t)
    (other-window 1)
    (eaf-open "about:blank" "browser" "dev_tools"))

  ;; hack eaf for evil-mode
  ;; send-key only in evil-emacs-state
  (add-hook 'eaf-mode-hook
            (lambda ()
              (company-mode -1)
              (setq-local evil-motion-state-map nil)
              ;; "C-z" normal-state -> emacs-state
              ;; ":" evil-ex
              (setq-local evil-normal-state-map
                          '(keymap
                            (58 . evil-ex)
                            (26 . evil-emacs-state)))))

  (defun eaf-switch-to-eww ()
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

  (defun eaf-buffer-names ()
    (mapcar #'buffer-name (eaf-buffers)))

  (defun eaf-buffers ()
    (let* ((all-buffers (cl-remove-if-not
                         (lambda (buffer)
                           (with-current-buffer buffer
                             (derived-mode-p 'eaf-mode)))
                         (buffer-list))))
      all-buffers))

  (defun +eaf/ivy-switch-buffer ()
    (interactive)
    (ivy-read "Switch to buffer: "
              (delete (buffer-name (current-buffer))
                      (eaf-buffer-names))
              :initial-input nil
              :action #'ivy--switch-buffer-action
              :caller '+eaf/ivy-switch-buffer)))



(provide 'init-eaf)

;;; init-eaf.el ends here
