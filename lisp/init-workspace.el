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
  (tab-bar-show 1)
  (tab-bar-new-tab-choice "*scratch*"))

(defun +workspace/tab-new (name)
  (interactive (list
                (read-from-minibuffer
                 "New Tab Name(*scratch*): "
                 nil nil nil nil (buffer-name))))
    (tab-new)
    (tab-rename name))

;; Easily persist and restore your Emacs editing sessions
(use-package easysession
  :commands (easysession-save-as easysession-switch-to)
  :config
  (when tab-bar-mode
    (advice-add 'easysession-switch-to :after
                (lambda (&optional session-name)
                  (tab-bar--update-tab-bar-lines)))))


(provide 'init-workspace)

;;; init-workspace.el ends here
