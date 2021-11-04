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

(use-package shell-pop
  :commands shell-pop
  :config
  (setq shell-pop-term-shell personal-shell-executable)
  (cond ((functionp 'vterm)
         (setq shell-pop-shell-type '("vterm" "*vterm*" (lambda () (vterm)))))
        (t (setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))))
  (setq shell-pop-window-position "bottom")
  ;; The last line is needed or no picked up by 'shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(defun +shell/projectile-shell-pop ()
  "Open a term buffer at projectile project root,
if no project root found, use current directory instead."
  (interactive)
  (let ((default-directory (or (+project/root) default-directory)))
    (call-interactively 'shell-pop)))

;; HIGHLY RECOMMENDED
;; https://github.com/akermu/emacs-libvterm
;; On ArchLinux or Manjaro install libvterm first
;; sudo pacman -S libvterm
(use-package vterm
  ;; :quelpa (vterm :fetcher github :repo "jixiuf/emacs-libvterm"
  ;;                :files (:defaults "*.c" "*.h" "CMakeLists.txt"))
  :if (and (executable-find "vterm-ctrl")
           (executable-find "make")
           (executable-find "cmake")
           (fboundp 'module-load))
  :commands (vterm vterm-other-window)
  :bind ((:map vterm-mode-map
               ("M-u" . ace-window)
               ("M-`" . +vterm/send-tmux-prefix-key)
               ("C-s" . +vterm/swiper)
               ("<f9>" . +vterm/toggle-here)
               ("<f10>" . +vterm/toggle-other-window)
               ("<f11>" . toggle-frame-fullscreen))
         (:map vterm-copy-mode-map
               ("q" . vterm-copy-mode-done)))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-term-environment-variable "xterm-24bit")
  (vterm-timer-delay 0.01)
  :init
  (global-set-key [f9] '+vterm/toggle-here)
  (global-set-key [f10] '+vterm/toggle-other-window)
  :config
  ;; https://github.com/akermu/emacs-libvterm/issues/58#issuecomment-516950648
  (with-eval-after-load 'doom-themes
    (set-face-background 'vterm-color-black (doom-color 'base6)))

  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'insert)
    (evil-define-key '(normal insert emacs) vterm-copy-mode-map "q" #'vterm-copy-mode-done)
    (defun +vterm/evil-esc ()
      (interactive)
      (vterm-send-escape)
      (evil-normal-state))
    (evil-define-key 'insert vterm-mode-map (kbd "<escape>") #'+vterm/evil-esc))

  (defvar +vterm/toggle--window-configration nil)

  (defun +vterm/toggle (arg &optional this-window-p)
    "Toggles a window at project root.

If prefix ARG is non-nil, cd into `default-directory' instead of project root."
    (unless (fboundp 'module-load)
      (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
    (let* ((dir-remote-p (file-remote-p default-directory))
           (default-directory (if arg
                                  default-directory
                                (or
                                 (when dir-remote-p nil)
                                 (projectile-project-root)
                                 default-directory)))
           (buffer-name
            (format "*vterm:<%s>*"
                    (file-name-nondirectory
                     (directory-file-name
                      (expand-file-name default-directory))))))
      (if-let (win (get-buffer-window buffer-name))
          (if (eq (selected-window) win)
              (progn
                (with-current-buffer (get-buffer buffer-name)
                  (bury-buffer))
                (when +vterm/toggle--window-configration
                  (set-window-configuration +vterm/toggle--window-configration)))
            (select-window win)
            (when (bound-and-true-p evil-local-mode)
              (evil-change-to-initial-state)))
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (unless (eq major-mode 'vterm-mode)
              (vterm-mode)
              (unless dir-remote-p
                (+vterm/activate-local-python-venv)))
            ;; (when dir-remote-p
            ;;   (+vterm/change-remote-directory))
            )
          (setq +vterm/toggle--window-configration (current-window-configuration))
          (if this-window-p
              (let ((new-window (split-window
                                 (selected-window)
                                 (round (* (window-height (frame-root-window)) 0.7))
                                 'below)))
                (select-window new-window)
                (switch-to-buffer buffer))
              (pop-to-buffer buffer))))))

  (defun +vterm/toggle-other-window (arg)
    "Toggles a terminal popup window at project root.
If prefix ARG is non-nil, cd into `default-directory' instead of project root."
    (interactive "P")
    (+vterm/toggle arg nil))

  (defun +vterm/toggle-here (arg)
    "Open a terminal buffer in the current window at project root.
If prefix ARG is non-nil, cd into `default-directory' instead of project root."
    (interactive "P")
    (+vterm/toggle arg t))

  (defun +vterm/change-remote-directory ()
    "Use the corresponding method to prepare vterm at the corresponding remote directory."
    (when (featurep 'tramp)
      (message "default-directory is %s" default-directory)
      (with-parsed-tramp-file-name default-directory path
        (let ((method (cadr (assoc `tramp-login-program
                                   (assoc path-method tramp-methods)))))
          (cond
           ((string-equal method "ssh")
            (progn
              (vterm-send-string
               (concat method " "
                       (when path-user (concat path-user "@"))
                       path-host
                       (when path-port (concat " -p " path-port))))
              (vterm-send-return)
              (vterm-send-string
               (concat "cd " path-localname))
              (vterm-send-return)))
           (t nil))))))

  (defun +vterm/activate-local-python-venv ()
    (when (file-exists-p (expand-file-name "venv" default-directory))
      (dolist (char (string-to-list "source venv/bin/activate"))
        (vterm--update vterm--term (char-to-string char) nil nil nil))
      (vterm-send-return)))

  (defun +vterm/send-tmux-prefix-key ()
    "Send `M-`' to the libvterm."
    (interactive)
    (when (and (featurep 'evil)
               (not (evil-insert-state-p)))
      (evil-insert-state))
    (vterm-send-key "`" nil t))

  (defun +vterm/swiper ()
    (interactive)
    (vterm-copy-mode)
    (message "vterm-copy-mode activated")
    (swiper)))

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

;; TODO: lossless keyboard input in terminal
;; https://github.com/CyberShadow/term-keys
;; (use-package term-keys
;;   :quelpa (term-keys :fetcher github :repo "CyberShadow/term-keys")
;;   :hook (after-init . term-keys-mode))

;; Note: Fixing Emacs tramp mode when using zsh
;; https://blog.karssen.org/2016/03/02/fixing-emacs-tramp-mode-when-using-zsh/
;; https://www.emacswiki.org/emacs/TrampMode#toc8

(use-package isend-mode
  :defer t)


(provide 'init-shell-term)

;;; init-shell-term.el ends here
