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
          ;; NOTE: press <super>-[num] select hint num tab
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
  (tab-new)
  (if (> (length name) 0)
      (tab-rename name)))

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
  ;; Only persist and restore visible buffers
  ;; https://github.com/jamescherti/easysession.el?tab=readme-ov-file#how-to-only-persist-and-restore-visible-buffers
  (defun my-easysession-visible-buffer-list ()
    "Return a list of all visible buffers in the current session.
This includes buffers visible in windows or tab-bar tabs."
    (let ((visible-buffers '()))
      (dolist (buffer (buffer-list))
        (when (or
               ;; Windows
               (get-buffer-window buffer 'visible)
               ;; Tab-bar windows
               (and (bound-and-true-p tab-bar-mode)
                    (fboundp 'tab-bar-get-buffer-tab)
                    (tab-bar-get-buffer-tab buffer t nil)))
          (push buffer visible-buffers)))
      visible-buffers))
  (setq easysession-buffer-list-function #'my-easysession-visible-buffer-list)

  (when tab-bar-mode
    (advice-add 'easysession-switch-to :after
                (lambda (&optional session-name)
                  (tab-bar--update-tab-bar-lines)))))


(provide 'init-workspace)

;;; init-workspace.el ends here
