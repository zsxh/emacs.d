;; init-workspace.el --- Workspace Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Workspace configuations
;;

;;; Code:

;; TODO: emacs built-in `tab-bar'
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :custom
  (tab-bar-show 1))

;; TODO: https://github.com/mclear-tools/tabspaces
(use-package tabspaces
  :defer t
  ;; :hook (after-init . tabspaces-mode)
  :custom
  ;; (tab-bar-show nil)

  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore nil))


(provide 'init-workspace)

;;; init-workspace.el ends here
