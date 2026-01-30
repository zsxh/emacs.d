;; init-lang-python.el --- python configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Python Configurations
;;

;;; Code:

;; NOTE: install python dev tools
;; - python language server `basedpyright'
;; - python linter `ruff'
;; - python formatter `black'

(when (and (version< emacs-version "31")
           (treesit-ready-p 'python))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;; Python virtual environment support for Emacs
(use-package pyvenv
  :commands pyvenv-activate)

(use-package python
  :ensure nil
  :hook (python-base-mode . eglot-ensure)
  ;; :custom (python-indent-offset 2)
  )

;; TODO: remove python ruff linter
;; (use-package flymake-ruff
;;   :defer t
;;   :hook (eglot-managed-mode . (lambda ()
;;                                 (when (derived-mode-p 'python-base-mode)
;;                                   (flymake-ruff-load)))))

(with-eval-after-load 'python
  ;; formatter
  (require 'reformatter)
  (reformatter-define black-format
    :program "black"
    :args '("-q" "-"))
  (reformatter-define ruff-format
    :program "ruff"
    :args '("--fix-only" "-"))
  (defun +python/format-buffer ()
    (interactive)
    (ruff-format-buffer)
    (black-format-buffer))

  ;; keybindings
  (+eglot/set-leader-keys python-mode-map)
  (+eglot/set-leader-keys python-ts-mode-map)
  (+funcs/major-mode-leader-keys
   '(python-mode-map python-ts-mode-map)
   "f" '(+python/format-buffer :which-key "format")
   "v" '(nil :which-key "venv")
   "va" '(pyvenv-activate :which-key "activate")
   "vd" '(pyvenv-deactivate :which-key "deactivate"))

  (defun +python/locate-venv-python-cmd ()
    "Look for virtual environments local to the workspace."
    (when-let* ((project-dir (+project/root))
                (venv-dir (or
                           (when-let* ((venv (locate-dominating-file project-dir "venv")))
                             (file-name-concat venv "venv"))
                           (when-let* ((venv (locate-dominating-file project-dir ".venv")))
                             (file-name-concat venv ".venv"))))
                (python-cmd (executable-find (file-name-concat venv-dir "bin" "python"))))
      python-cmd))

  (require 'eglot)

  (defvar eglot-python-workspace-configuration
    '(:ty
      (:experimental
       (:rename t
        :autoImport t))))

  (setq-default eglot-workspace-configuration
                (plist-put eglot-workspace-configuration :ty
                           (plist-get eglot-python-workspace-configuration :ty))))


(provide 'init-lang-python)

;;; init-lang-python.el ends here
