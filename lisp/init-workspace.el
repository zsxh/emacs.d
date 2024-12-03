;; init-workspace.el --- Workspace Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Workspace configuations
;;

;;; Code:

;; Workspace
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :init
  (setopt tab-bar-show 1
          ;; press <super>-[num] select hint num tab
          tab-bar-select-tab-modifiers '(super))
  (setq tab-bar-new-tab-choice "*scratch*"
        tab-bar-auto-width nil
        tab-bar-tab-hints t
        tab-bar-new-tab-to 'rightmost))

(defun +workspace/tab-new (name)
  (interactive (list
                (read-from-minibuffer
                 "New Tab Name(*scratch*): "
                 nil nil nil nil (buffer-name))))
  (let* ((tab-explicit-name (> (length name) 0))
         (tab-name (if tab-explicit-name
                       name
                     "*scratch*")))
    (tab-new)
    (tab-rename tab-name)))

;; Buffer Tab
(use-package tab-line
  ;; :hook (after-init . global-tab-line-mode)
  :defer t)

(use-package tab-line-nerd-icons
  :hook (global-tab-line-mode . tab-line-nerd-icons-global-mode))

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
