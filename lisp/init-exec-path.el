;; init-exec-path.el --- exec-path configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  exec-path configurations
;;

;;; Code:

(defvar exec-path-from-shell-initialize-p nil)
(defvar nodejs-version "v12.13.0")

(use-package exec-path-from-shell
  :if (not (file-remote-p default-directory))
  :defer 1
  :config
  ;; Performance improvement: I already set environment variable in .zshenv,so no need to check .zshrc
  (setq exec-path-from-shell-check-startup-files nil)

  (defun exec-path-from-shell-initialize-async ()
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`\\(load-path\\)\\'")
        (require 'exec-path-from-shell)
        (setq exec-path-from-shell-check-startup-files nil)
        (exec-path-from-shell-getenvs exec-path-from-shell-variables))
     (lambda (pairs)
       (when exec-path-from-shell-check-startup-files
         (exec-path-from-shell--maybe-warn-about-startup-files pairs))
       (mapc (lambda (pair)
               (exec-path-from-shell-setenv (car pair) (cdr pair)))
             pairs)
       ;; zsh-defer load_nvm no matter if it's in emacs process, so i put node path manually
       (let ((nvm-execute-path (file-truename (format "~/.nvm/versions/node/%s/bin" nodejs-version))))
         (setq exec-path (cons nvm-execute-path exec-path))))))

  (defun +env/load-shell-env ()
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize-async)
      (exec-path-from-shell-copy-envs '("LANG" "LD_LIBRARY_PATH"))
      (setq exec-path-from-shell-initialize-p t)))

  ;; Initialize
  (+env/load-shell-env)

  (defun exec-path-from-shell-advice ()
    (if exec-path-from-shell-initialize-p
        (message "All shell environment variables already loaded in Emacs!")
      (+env/load-shell-env)))

  (advice-add #'exec-path-from-shell-initialize :override #'exec-path-from-shell-advice))


(provide 'init-exec-path)

;;; init-exec-path.el ends here
