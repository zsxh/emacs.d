;; init-experimental.el --- Experimental Feature	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Zsxh Chen

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;;  Experimental Feature
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(let ((default-directory (expand-file-name "experimental" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

;; https://github.com/manateelazycat/emacs-application-framework
(use-package eaf
  :commands (eaf-open eaf-open-url)
  :preface
  (defun eaf-qutebrowser ()
    (interactive)
    (eaf-open "eaf-qutebrowser"))
  :config
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
                   (string-equal buffer-app-name "browser"))
              (progn
                (let ((key-desc (key-description (make-vector 1 last-command-event)))
                      (target-desc nil))
                  (when (member key-desc '("j" "k"))
                    (cond
                     ((string-equal key-desc "j")
                      (eaf-call "send_key" buffer-id "<down>"))
                     ((string-equal key-desc "k")
                      (eaf-call "send_key"  buffer-id "<up>")))
                    (setq last-command-event nil)))))

             ;; for pdfviewer
             ((and (eq major-mode 'eaf-mode)
                   (not (evil-emacs-state-p))
                   (string-equal buffer-app-name "pdfviewer"))
              (progn
                (let* ((event last-command-event)
                       (key (make-vector 1 event))
                       (key-command (format "%s" (key-binding key)))
                       (key-desc (key-description key)))
                  (when (or
                         (equal key-command "digit-argument")
                         (member key-desc '("j" "k" "f" "b" "," "." "t" "-" "=" "0" "g" "p" "[" "]")))
                    (if (string-equal key-desc "f")
                        (eaf-call "send_key" buffer-id "SPC")
                      (eaf-call "send_key" buffer-id key-desc))
                    (setq last-command-event nil)))))

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
                   ;; Just send event when user insert single character.
                   ;; Don't send event 'M' if user press Ctrl + M.
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

;; Better eshell
;; https://github.com/manateelazycat/aweshell
;; (use-package aweshell
;;   :commands aweshell-new
;;   :quelpa ((aweshell :fetcher github :repo "manateelazycat/aweshell")))

;; This extension will ask me Chinese words and then insert translation as variable or function name.
;; https://github.com/manateelazycat/insert-translated-name
(use-package insert-translated-name
  :commands insert-translated-name-insert
  :quelpa ((insert-translated-name :fetcher github :repo "manateelazycat/insert-translated-name")))

;; Use system-wide command instead now
;; Simplified and community-driven man pages
;; (use-package tldr
;;   :ensure t
;;   :commands tldr)


(provide 'init-experimental)

;;; init-experimental.el ends here
