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
;; https://github.com/manateelazycat/emacs-application-framework#install-eaf
(use-package eaf
  :load-path "~/.emacs.d/submodules/emacs-application-framework"
  :commands (eaf-open
             eaf-open-browser
             eaf-open-dash
             eaf-open-bookmark)
  :hook (eaf-mode . (lambda () (setq left-fringe-width 0
                                     right-fringe-width 0)))
  :init
  (progn
    ;; Pdf viewer settings
    (add-to-list 'auto-mode-alist
                 '("\\.pdf\\'" . (lambda ()
                                   (let ((filename buffer-file-name))
                                     (eaf-open filename)
                                     (kill-buffer (file-name-nondirectory filename))))))
    (with-eval-after-load 'org
      (setq browse-url-browser-function 'eaf-open-browser)))
  :config
  (setq eaf-browser-default-search-engine "duckduckgo"
        eaf-config-location (expand-file-name (locate-user-emacs-file ".cache/eaf")))

  ;; (setq eaf-enable-debug t
  ;;       eaf-python-command "/home/zsxh/.pyenv/versions/miniconda3-latest/bin/python3")

  (eaf-setq eaf-browser-blank-page-url "https://duckduckgo.com")
  (eaf-setq eaf-browser-default-zoom "1.2")
  (eaf-setq eaf-browser-remember-history "true")

  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)

  (eaf-bind-key eaf-switch-to-eww "C-t" eaf-browser-keybinding)
  (eaf-bind-key nil "M-u" eaf-browser-keybinding)
  (eaf-bind-key clear_focus "M-p" eaf-browser-keybinding)
  (eaf-bind-key nil "T" eaf-browser-keybinding)
  (eaf-bind-key recover_prev_close_page "X" eaf-browser-keybinding)

  (require 'dash)
  (when personal-eaf-grip-token
    (setq eaf-grip-token personal-eaf-grip-token))

  (when personal-http-proxy
    (let* ((host-port (split-string personal-http-proxy ":"))
           (host (nth 0 host-port))
           (port (nth 1 host-port)))
      (setq eaf-proxy-host host
            eaf-proxy-port port
            eaf-proxy-type "http")))

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
