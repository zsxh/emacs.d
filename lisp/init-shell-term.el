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

(global-set-key [f9] '+vterm/toggle)

;; https://github.com/akermu/emacs-libvterm
;; Linux: sudo pacman -S libvterm
;; MacOS: brew install libvterm
(use-package vterm
  :commands (vterm vterm-other-window)
  :bind ((:map vterm-mode-map
          ("M-u" . ace-window)
          ("M-`" . +vterm/send-tmux-prefix-key)
          ("C-s" . +vterm/search-line)
          ("<f9>" . +vterm/toggle)
          ("<f11>" . toggle-frame-fullscreen))
         (:map vterm-copy-mode-map
          ("q" . vterm-copy-mode-done)))
  :config
  (setq vterm-timer-delay nil)

  ;; https://github.com/akermu/emacs-libvterm#fonts
  (defun +vterm/set-font ()
    (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
    (buffer-face-mode t))
  (add-hook 'vterm-mode-hook #'+vterm/set-font)

  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'insert)
    (evil-define-key '(normal insert emacs) vterm-copy-mode-map "q" #'vterm-copy-mode-done)
    (defun +vterm/evil-esc ()
      (interactive)
      (vterm-send-escape)
      (evil-normal-state))
    (evil-define-key 'insert vterm-mode-map (kbd "<escape>") #'+vterm/evil-esc))

  (defun +vterm/toggle (arg)
    "Toggles a window at project root.

If prefix ARG is non-nil, cd into `default-directory' instead of project root."
    (interactive "P")
    (unless (fboundp 'module-load)
      (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
    (let* ((dir-remote-p (file-remote-p default-directory))
           (default-directory (if (or arg dir-remote-p)
                                  default-directory
                                (or (+project/root)
                                    default-directory)))
           (buffer-name
            (format "*vterm:<%s>*"
                    (file-name-nondirectory
                     (directory-file-name
                      (expand-file-name default-directory))))))
      (if-let* ((win (get-buffer-window buffer-name)))
          ;; vterm buffer exist
          (if (eq (selected-window) win)
              ;; hide selected vterm buffer
              (with-current-buffer (get-buffer buffer-name)
                (bury-buffer))
            ;; selected vterm buffer
            (select-window win))
        (if-let* ((buffer (get-buffer buffer-name)))
            ;; popup vterm buffer
            (display-buffer buffer)
          ;; create new vterm buffer
          (with-current-buffer (vterm buffer-name)
            (when (bound-and-true-p evil-local-mode)
              (evil-change-to-initial-state)))))))

  (defun +vterm/send-tmux-prefix-key ()
    "Send `M-`' to the libvterm."
    (interactive)
    (when (and (featurep 'evil)
               (not (evil-insert-state-p)))
      (evil-insert-state))
    (vterm-send-key "`" nil t))

  (defun +vterm/search-line ()
    (interactive)
    (vterm-copy-mode)
    ;; (message "vterm-copy-mode activated")
    (consult-line))

  ;; Kill vterm window and buffer when a vterm process is finished
  (add-hook 'vterm-exit-functions
            (lambda (_ _)
              (let* ((buffer (current-buffer))
                     (window (get-buffer-window buffer)))
                (when (not (one-window-p))
                  (delete-window window))
                (kill-buffer buffer)))))

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


(provide 'init-shell-term)

;;; init-shell-term.el ends here
