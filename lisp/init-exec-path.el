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
  :defer 1
  :ensure t
  :config
  ;; Performance improvement: I already set environment variable in .zshenv,so no need to check .zshrc
  (setq exec-path-from-shell-check-startup-files nil)

  ;; Lazy load Node Version Manager since its long boot time.
  ;; https://github.com/nvm-sh/nvm/issues/1277#issuecomment-470490227
  ;; The only inconvenience for me is the need to run node, nvm, or npm before any global package (the ones installed with npm install -g <package>), otherwise the command is not recognized.
  (defun +env/set-nvm-path ()
    (if (file-exists-p "~/.nvm/nvm.sh")
        (let* ((str (shell-command-to-string "nvm current"))
               (nvm-version (if (string-match "[ \t\n\r]+\\'" str) (replace-match "" t t str) str))
               (nvm-execute-path (file-truename (format "~/.nvm/versions/node/%s/bin" nvm-version))))
          (setq exec-path (cons nvm-execute-path exec-path)))))

  (defun +env/load-shell-env ()
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)
      (+env/set-nvm-path)
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
