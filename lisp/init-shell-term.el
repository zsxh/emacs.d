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
  :ensure t
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
  (let ((default-directory (or  (projectile-project-root) default-directory)))
    (call-interactively 'shell-pop)))

;; HIGHLY RECOMMENDED
;; https://github.com/akermu/emacs-libvterm
;; On ArchLinux or Manjaro install libvterm first
;; sudo pacman -S libvterm
(use-package vterm
  :ensure t
  ;; :quelpa (vterm :fetcher github :repo "jixiuf/emacs-libvterm"
  ;;                :files (:defaults "*.c" "*.h" "CMakeLists.txt"))
  :if (and (executable-find "vterm-ctrl")
           (executable-find "make")
           (executable-find "cmake")
           (fboundp 'module-load))
  :commands (vterm vterm-other-window +vterm/new +vterm/ivy-switch-buffer)
  :bind ((:map vterm-mode-map
               ("M-u" . ace-window)
               ("C-s" . swiper)
               ("<f11>" . toggle-frame-fullscreen)))
  :config
  (defun +vterm/auto-exit (buf)
    (when buf (kill-buffer buf)))

  (add-hook 'vterm-exit-functions #'+vterm/auto-exit)

  (defun +vterm/generate-buffer-name ()
    (format "*vterm: %s*" (file-name-nondirectory (directory-file-name default-directory))))

  (defun +vterm/new ()
    "Create a new vterm with `default-directory' buffer name."
    (interactive)
    (vterm (+vterm/generate-buffer-name)))

  (defun +vterm/new-other-window ()
    "Create a new vterm with `default-directory' buffer name in other window."
    (interactive)
    (let ((buffer (generate-new-buffer (+vterm/generate-buffer-name))))
      (with-current-buffer buffer
        (vterm-mode))
      (pop-to-buffer buffer)))

  (defun +vterm/get-project-buf ()
    (let* ((default-directory (or (projectile-project-root) default-directory))
           (buf-name (+vterm/generate-buffer-name))
           (buf (get-buffer buf-name)))
      buf))

  (defun +vterm/ivy-switch-buffer ()
    (interactive)
    (ivy-read "Switch to vterm buffer: "
              (delete (buffer-name (current-buffer))
                      (mapcar #'buffer-name
                              (cl-remove-if-not
                               (lambda (buffer)
                                 (with-current-buffer buffer
                                   (derived-mode-p 'vterm-mode)))
                               (buffer-list))))
              :initial-input nil
              :action #'ivy--switch-buffer-action
              :caller '+vterm/ivy-switch-buffer))

  ;; https://github.com/akermu/emacs-libvterm/issues/58#issuecomment-516950648
  (with-eval-after-load 'doom-themes
    (set-face-background 'vterm-color-black (doom-color 'base6)))

  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'insert))

  (with-eval-after-load 'paren
    (add-hook 'vterm-mode-hook 'locally-disable-show-paren)))

;; TODO: add `vterm-toggle' package
;; https://github.com/jixiuf/vterm-toggle
(use-package vterm-toggle
  :ensure t
  :commands (vterm-toggle vterm-toggle-cd)
  :init
  ;; (global-set-key [f2] 'vterm-toggle)          ; recent or current dir
  (global-set-key [C-f10] 'vterm-toggle-cd)     ; new current dir
  (global-set-key [f10] '+vterm/toggle-project) ; project root(initial one)
  :bind ((:map vterm-mode-map
               ;; ("<f2>" . vterm-toggle)
               ("<f10>" . +vterm/toggle-project)))
  :config
  (setq vterm-toggle-fullscreen-p nil)

  (defun +vterm/toggle-new ()
    "New vterm buffer."
    (if vterm-toggle-fullscreen-p
        (+vterm/new)
      (+vterm/new-other-window)))

  (defun +vterm/toggle--get-buffer (&optional make-cd ignore-prompt-p args)
    "Get vterm buffer.
Optional argument MAKE-CD make cd or not.
Optional argument ARGS optional args."
    (if vterm-toggle-use-dedicated-buffer
        (vterm-toggle--get-dedicated-buffer)
      ;; for now, args doesn't mean anything in vterm-toggle,
      ;; so, i use it as project identification.
      (or (and args (+vterm/get-project-buf))
          (vterm-toggle--recent-vterm-buffer make-cd ignore-prompt-p args))))

  (advice-add 'vterm-toggle--new :override '+vterm/toggle-new)
  (advice-add 'vterm-toggle--get-buffer :override '+vterm/toggle--get-buffer)

  (defun +vterm/toggle-project ()
    (interactive)
    (let* ((default-directory (or (projectile-project-root) default-directory))
           (buf-name (+vterm/generate-buffer-name))
           (buf (get-buffer buf-name)))
      (if buf
          (vterm-toggle t)
        (vterm-toggle-cd)))))

(use-package term
  :ensure nil
  :defer t
  :config
  ;; https://oremacs.com/2015/01/01/three-ansi-term-tips/
  (setq explicit-shell-file-name personal-shell-executable)

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
;;   :quelpa ((term-keys :fetcher github :repo "CyberShadow/term-keys"))
;;   :hook (after-init . term-keys-mode))

;; Note: Fixing Emacs tramp mode when using zsh
;; https://blog.karssen.org/2016/03/02/fixing-emacs-tramp-mode-when-using-zsh/
;; https://www.emacswiki.org/emacs/TrampMode#toc8


(provide 'init-shell-term)

;;; init-shell-term.el ends here
