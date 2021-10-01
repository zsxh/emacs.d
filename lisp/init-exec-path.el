;; init-exec-path.el --- exec-path configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  exec-path configurations
;;

;;; Code:

(defvar exec-path-from-shell-initialize-p nil)

(use-package exec-path-from-shell
  :if (not (file-remote-p default-directory))
  :defer 1
  :config
  (setq
   ;; NOTE: https://github.com/purcell/exec-path-from-shell#making-exec-path-from-shell-faster
   ;; load from .zshenv, no need to load .zshrc(interactive login shell)
   exec-path-from-shell-arguments nil
   exec-path-from-shell-variables '("PATH" "MANPATH" "LANG" "LANGUAGE" "LD_LIBRARY_PATH" "LSP_USE_PLISTS"))

  (defun exec-path-from-shell-initialize-async ()
    (async-start
     `(lambda ()
        ;; (rx bos (or "load-path" "exec-path-from-shell-arguments" "exec-path-from-shell-variables") eos)
        ,(async-inject-variables "\\`\\(?:exec-path-from-shell-\\(?:\\(?:argument\\|variable\\)s\\)\\|load-path\\)\\'")
        (require 'exec-path-from-shell)
        (exec-path-from-shell-getenvs exec-path-from-shell-variables))
     (lambda (pairs)
       (mapc (lambda (pair)
               (exec-path-from-shell-setenv (car pair) (cdr pair)))
             pairs)
       (message "[INFO]: init-exec-path.el shell environment variables loaded"))))

  (defun exec-path-from-shell-initialize-sync ()
    (exec-path-from-shell-copy-envs exec-path-from-shell-variables)
    (message "[INFO]: init-exec-path.el shell environment variables loaded"))

  ;; since I use fnm replacing zsh-defer + nvm,
  ;; no need to load shell variables asynchronously now
  (defun +env/load-shell-env ()
    (when (memq window-system '(mac ns x pgtk))
      ;; (exec-path-from-shell-initialize-async)
      (exec-path-from-shell-initialize-sync)
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
