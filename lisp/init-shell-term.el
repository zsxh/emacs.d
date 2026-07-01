;; init-shell-term.el --- Shell and Terminal Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Shell and Terminal Configurations
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(global-set-key [f9] '+ghostel/toggle)

(use-package term
  :ensure nil
  :defer t
  :config
  ;; https://oremacs.com/2015/01/01/three-ansi-term-tips/
  (setq shell-file-name personal-shell-executable)

  (defun +term/term-exec-hook ()
    (let* ((buff (current-buffer))
           (proc (get-buffer-process buff)))
      (set-process-sentinel
       proc
       `(lambda (process event)
          (if (string= event "finished\n")
              (kill-buffer ,buff))))))
  (add-hook 'term-exec-hook '+term/term-exec-hook)

  (with-eval-after-load 'evil
    (evil-define-key 'normal term-raw-map "p" 'term-paste)
    (evil-define-key 'insert term-raw-map "\C-y" 'term-paste)))

;; Support for the Kitty Keyboard protocol in Emacs
;; If you want to know if your terminal supports kkp, if its activated,
;; and if yes, which enhancements are active, use `kkp-status'.
(use-package kkp
  :if (not (display-graphic-p))
  :hook (after-init . global-kkp-mode))

(use-package eat
  :commands (eat-toggle)
  :config
  (unless (file-exists-p eat-term-terminfo-directory)
    (eat-compile-terminfo))
  (setq eat-kill-buffer-on-exit t
        eat-enable-shell-prompt-annotation nil
        eat-enable-shell-command-history nil
        eat-minimum-latency 0.003
        eat-maximum-latency 0.033)
  (with-eval-after-load 'evil
    (evil-set-initial-state 'eat-mode 'insert))

  (defun eat-toggle (arg)
    (interactive "P")
    (let* ((dir-remote-p (file-remote-p default-directory))
           (project-root (and (not arg)
                              (not dir-remote-p)
                              (+project/root)))
           (default-directory (expand-file-name (or project-root default-directory)))
           (eat-buffer-name (format "*eat:<%s>*"
                                    (file-name-nondirectory
                                     (directory-file-name
                                      (expand-file-name default-directory)))))
           (buf (get-buffer eat-buffer-name)))
      (cond
       ((buffer-live-p buf)
        (if (eq (selected-window) (get-buffer-window buf))
            (with-current-buffer buf (bury-buffer))
          (display-buffer buf)))
       (t (eat))))))

;; Interact with tmux from Emacs.
(use-package emamux
  :defer t)

;; [Terminal emulator powered by libghostty](https://github.com/dakra/ghostel)
(use-package ghostel
  :defer t
  :bind ((:map ghostel-mode-map
          ("M-`" . +ghostel/send-tmux-prefix-key)
          ("<f9>" . +ghostel/toggle)))
  :init
  (setopt ghostel-keymap-exceptions
          '("C-c" "C-x" "C-u" "C-h" "M-x" "M-o" "M-:" "C-\\"
            "<f9>" "M-u" "C-y" "C-s"))
  :config
  (ghostel-compile-global-mode 1)
  (add-to-list 'ghostel-compile-global-mode-excluded-modes 'rg-mode)

  (defun +ghostel/send-tmux-prefix-key ()
    "Send `M-`' to the ghostel."
    (interactive)
    (when (and (featurep 'evil)
               (not (evil-insert-state-p)))
      (evil-insert-state))
    ;; (ghostel-send-key "`" "meta")
    (ghostel--send-string "`")))

(defun +ghostel/toggle (arg)
  "Toggles a window at project root.

If prefix ARG is non-nil, cd into `default-directory' instead of project root."
  (interactive "P")
  (let* ((wins (window-list))
         (ghostel-win (cl-find-if
                       (lambda (win)
                         (with-current-buffer (window-buffer win)
                           (eq major-mode 'ghostel-mode)))
                       wins)))
    (if ghostel-win
        (if (eq ghostel-win (selected-window))
            (with-current-buffer (window-buffer ghostel-win)
              (bury-buffer))
          (select-window ghostel-win))
      (let* ((dir-remote-p (file-remote-p default-directory))
             (proj-p (and (not (or arg dir-remote-p))
                          (project-current nil))))
        (if proj-p
            (call-interactively 'ghostel-project)
          (call-interactively 'ghostel))))))

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode)
  :config
  (evil-define-key* '(insert motion) evil-ghostel-mode-map
    (kbd "C-s") 'consult-line
    (kbd "C-y") 'yank
    (kbd "C-z") 'evil-normal-state))


(provide 'init-shell-term)

;;; init-shell-term.el ends here
