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
  :commands (vterm vterm-other-window +vterm/with-name +vterm/ivy-switch-buffer)
  :bind (:map vterm-mode-map
              ("M-u" . ace-window)
              ("C-s" . swiper))
  :config
  (defun +vterm/auto-exit (buf)
    (when buf (kill-buffer buf)))

  (add-hook 'vterm-exit-functions #'+vterm/auto-exit)

  (defun +vterm/with-name ()
    "Create a new vterm with `default-directory' buffer name"
    (interactive)
    (vterm (format "*vterm: %s*" (file-name-nondirectory (directory-file-name default-directory)))))

  (defun +vterm/ivy-switch-buffer ()
    (interactive)
    (ivy-read "Switch to vterm buffer: "
              (delete (buffer-name (current-buffer))
                      (mapcar #'buffer-name
                              (cl-remove-if-not
                               (lambda (buffer)
                                 (let ((name (buffer-name buffer)))
                                   (string-prefix-p "*vterm:" name)))
                               (buffer-list))))
              :initial-input nil
              :action #'ivy--switch-buffer-action
              :caller '+vterm/ivy-switch-buffer))

  ;; https://github.com/akermu/emacs-libvterm/issues/58#issuecomment-516950648
  (with-eval-after-load 'doom-themes
    (set-face-background 'vterm-color-black (doom-color 'base6)))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'insert)))

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

;; Better eshell
;; https://github.com/manateelazycat/aweshell
;; (use-package aweshell
;;   :commands aweshell-new
;;   :quelpa ((aweshell :fetcher github :repo "manateelazycat/aweshell")))


(provide 'init-shell-term)

;;; init-shell-term.el ends here
