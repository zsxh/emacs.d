;; init-workspace.el --- Workspace Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Workspace configuations
;;

;;; Code:

;; emacs built-in `tab-bar'
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-show 1))

(defun +workspace/tab-new (name)
    (interactive (list (read-from-minibuffer
                        (format "New Tab Name(%s): " (buffer-name))
                        nil nil nil nil (buffer-name))))
    (tab-new)
    (tab-rename name))

;; Easily persist and restore your Emacs editing sessions
(use-package easysession
  :commands (easysession-save-as easysession-switch-to))


(provide 'init-workspace)

;;; init-workspace.el ends here
