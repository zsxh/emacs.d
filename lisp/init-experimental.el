;; init-experimental.el --- Experimental Feature	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Experimental Feature
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; https://github.com/manateelazycat/emacs-application-framework
(use-package eaf
  :commands (eaf-open eaf-open-url)
  :preface
  (defun eaf-qutebrowser ()
    (interactive)
    (eaf-open "eaf-qutebrowser"))
  :config
  (require 'dash)
  (when personal-eaf-grip-token
    (setq eaf-grip-token personal-eaf-grip-token))

  ;; hack eaf for evil-mode
  ;; send-key only in evil-emacs-state
  (add-hook 'eaf-mode-hook
            (lambda ()
              (setq-local evil-motion-state-map nil)
              ;; "C-z" normal-state -> emacs-state
              ;; ":" evil-ex
              (setq-local evil-normal-state-map
                          '(keymap
                            (58 . evil-ex)
                            (26 . evil-emacs-state)))))

  (defun eaf-monitor-key-event ()
    (unless
        (ignore-errors
          (with-current-buffer (buffer-name)
            (cond
             ;; 为了 helm-dash browser 为 eaf 时,设置j->down,k->up
             ((and (eq major-mode 'eaf-mode)
                   (not (evil-emacs-state-p))
                   (or (string-equal buffer-app-name "browser")
                       (string-equal buffer-app-name "markdownpreviewer")))
              (progn
                (let ((key-desc (key-description (make-vector 1 last-command-event)))
                      (target-desc nil))
                  ;; (message "press %s" key-desc)
                  (cond
                   ((string-equal key-desc "h")
                    (eaf-call "send_key" buffer-id "<left>"))
                   ((string-equal key-desc "j")
                    (eaf-call "send_key" buffer-id "<down>"))
                   ((string-equal key-desc "k")
                    (eaf-call "send_key" buffer-id "<up>"))
                   ((string-equal key-desc "l")
                    (eaf-call "send_key" buffer-id "<right>"))
                   ((string-equal key-desc "u")
                    (eaf-call "send_key" buffer-id "<prior>"))
                   ((string-equal key-desc "d")
                    (eaf-call "send_key" buffer-id "<next>"))
                   ((string-equal key-desc "H")
                    (eaf-call "send_keystroke" buffer-id "M-b"))
                   ((string-equal key-desc "F")
                    (eaf-call "send_keystroke" buffer-id "M-f"))
                   ((member key-desc '("-" "=" "0"))
                    (eaf-call "send_keystroke" buffer-id (format "C-%s" key-desc)))
                   ((string-equal key-desc "T")
                    (progn
                      (let* ((url buffer-url)
                             (eww-buffer (car (-filter (lambda (buffer)
                                                         (with-current-buffer buffer
                                                           (and (derived-mode-p 'eww-mode)
                                                                (equal url (plist-get eww-data :url)))))
                                                       (buffer-list)))))
                        (if eww-buffer
                            (switch-to-buffer eww-buffer)
                          (eww url)))))))
                (setq last-command-event nil)))

             ;; for pdfviewer
             ((and (eq major-mode 'eaf-mode)
                   (not (evil-emacs-state-p))
                   (string-equal buffer-app-name "pdfviewer"))
              (progn
                (let* ((event last-command-event)
                       (key (make-vector 1 event))
                       (key-command (format "%s" (key-binding key)))
                       (key-desc (key-description key)))
                  ;; (message "press %s" key-desc)
                  (cond
                   ((string-equal key-desc "d")
                    (eaf-call "send_key" buffer-id "SPC"))
                   ((string-equal key-desc "u")
                    (eaf-call "send_key" buffer-id "b"))
                   ((or
                     (equal key-command "digit-argument")
                     (member key-desc '("j" "k" "," "." "t" "-" "=" "0" "g" "p" "[" "]")))
                    (eaf-call "send_key" buffer-id key-desc))))
                (setq last-command-event nil)))

             ;; 其余根据evil mode state 再决定
             ((and (eq major-mode 'eaf-mode)
                   (evil-emacs-state-p))
              (progn
                (let* ((event last-command-event)
                       (key (make-vector 1 event))
                       (key-command (format "%s" (key-binding key)))
                       (key-desc (key-description key)))

                  ;; Uncomment for debug.
                  ;; (message (format "!!!!! %s %s %s %s" event key key-command key-desc))

                  (cond
                   ;; Just send event when user insert single character. ;; Don't send event 'M' if user press Ctrl + M.
                   ((and
                     (or
                      (equal key-command "self-insert-command")
                      (equal key-command "completion-select-if-within-overlay"))
                     (equal 1 (string-width (this-command-keys))))
                    (eaf-call "send_key" buffer-id key-desc))
                   ((string-match "^[CMSs]-.*" key-desc)
                    (eaf-call "send_keystroke" buffer-id key-desc))
                   ((or
                     (equal key-command "nil")
                     (equal key-desc "RET")
                     (equal key-desc "DEL")
                     (equal key-desc "TAB")
                     (equal key-desc "SPC")
                     (equal key-desc "<backtab>")
                     (equal key-desc "<home>")
                     (equal key-desc "<end>")
                     (equal key-desc "<left>")
                     (equal key-desc "<right>")
                     (equal key-desc "<up>")
                     (equal key-desc "<down>")
                     (equal key-desc "<prior>")
                     (equal key-desc "<next>"))
                    (eaf-call "send_key" buffer-id key-desc))
                   (t
                    (unless (or
                             (equal key-command "keyboard-quit")
                             (equal key-command "kill-this-buffer")
                             (equal key-command "eaf-open"))
                      (ignore-errors (call-interactively (key-binding key)))))))
                ;; Set `last-command-event' with nil, emacs won't notify me buffer is ready-only,
                ;; because i insert nothing in buffer.
                (setq last-command-event nil))))))
      ;; If something wrong in `eaf-monitor-key-event', emacs will remove `eaf-monitor-key-event' from `pre-command-hook' hook list.
      ;; Then we add `eaf-monitor-key-event' in `pre-command-hook' list again, hahahaha.
      (run-with-timer
       0.1
       nil
       (lambda ()
         (progn
           (add-hook 'pre-command-hook #'eaf-monitor-key-event)))))))


;; Pdf viewer settings
(add-to-list 'auto-mode-alist
             '("\\.pdf\\'" . (lambda ()
                               (let ((filename buffer-file-name))
                                 (eaf-open filename)
                                 (kill-buffer (file-name-nondirectory filename))))))


;; Use system-wide command instead now
;; Simplified and community-driven man pages
;; (use-package tldr
;;   :ensure t
;;   :commands tldr)


;; FIXME: wait until emacs xwdigets work well ...
(use-package webkit-katex-render
  :if (featurep 'xwidget-internal)
  :quelpa ((webkit-katex-render
            :fetcher github
            :repo "fuxialexander/emacs-webkit-katex-render"
            :files (:defaults "katex.html")))
  :commands webkit-katex-render-mode)


(provide 'init-experimental)

;;; init-experimental.el ends here
