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

;; https://github.com/manateelazycat/emacs-application-framework
(use-package eaf
  :load-path "~/.emacs.d/submodules/emacs-application-framework"
  :commands (eaf-open eaf-open-browser eaf-open-dash)
  :config
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
    (let* ((url eaf--buffer-url)
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

  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)

  ;; TODO: modify current local map

  ;; (eaf-bind-key eaf-browser-toggle-keybinding "C-t" eaf-browser-keybinding)

  ;; (setq eaf-browser-last-keybinding eaf-browser-keybinding)

  ;; (setq eaf-browser-keybinding
  ;;       '(("-" . "zoom_out")
  ;;         ("=" . "zoom_in")
  ;;         ("0" . "zoom_reset")
  ;;         ("C-s" . "search_text_forward")
  ;;         ("C-r" . "search_text_backward")
  ;;         ("j" . "scroll_up")
  ;;         ("k" . "scroll_down")
  ;;         ("C-v" . "scroll_up_page")
  ;;         ("C-y" . "yank_text")
  ;;         ("C-w" . "kill_text")
  ;;         ("f" . "open_link")
  ;;         ("F" . "open_link_new_tab")
  ;;         ("C-/" . "undo_action")
  ;;         ("M-_" . "redo_action")
  ;;         ("M-w" . "copy_text")
  ;;         ("M-f" . "history_forward")
  ;;         ("M-b" . "history_backward")
  ;;         ("M-q" . "clean_all_cookie")
  ;;         ("M-v" . "scroll_down_page")
  ;;         ("M-<" . "scroll_to_begin")
  ;;         ("M->" . "scroll_to_bottom")
  ;;         ("<f5>" . "refresh_page")
  ;;         ("T" . eaf-switch-to-eww)
  ;;         ("C-t" . eaf-browser-toggle-keybinding)))

  ;; (defun eaf-browser-toggle-keybinding ()
  ;;   (interactive)
  ;;   (let* ((temp eaf-browser-keybinding))
  ;;     (setq eaf-browser-keybinding eaf-browser-last-keybinding
  ;;           eaf-browser-last-keybinding temp)
  ;;     (eaf--gen-keybinding-map (symbol-value 'eaf-browser-keybinding))
  ;;     (print eaf-browser-keybinding)
  ;;     (with-current-buffer (current-buffer)
  ;;       (setq-local emulation-mode-map-alists
  ;;                   (default-value 'emulation-mode-map-alists))
  ;;       (push (list (cons t eaf-mode-map))
  ;;             emulation-mode-map-alists)
  ;;       (push 'evil-mode-map-alist emulation-mode-map-alists))))
  )


;; Pdf viewer settings
(add-to-list 'auto-mode-alist
             '("\\.pdf\\'" . (lambda ()
                               (let ((filename buffer-file-name))
                                 (eaf-open filename)
                                 (kill-buffer (file-name-nondirectory filename))))))


(provide 'init-eaf)

;;; init-eaf.el ends here
