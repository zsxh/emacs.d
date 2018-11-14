;; init-python.el --- python configurations	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Zsxh Chen

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;;  Python Configurations
;;

;;; Code:

;; Somehow pipenv cant really active virtulenv, but i found pyvenv do a good job for this
(use-package pyvenv
  :ensure t
  :commands pyvenv-activate)

(use-package pyenv-mode
  :ensure t
  :commands pyenv-mode
  :hook (python-mode . pyenv-mode))

(use-package pipenv
  :ensure t
  :commands pipenv-mode
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended))

(use-package lsp-python
  :ensure t
  :requires init-lsp
  :commands lsp-python-enable
  :preface
  (defun +python/lsp-python-config ()
    (setq-local python-indent-offset 2)
    (setq-local company-backends
                '((company-lsp :separate company-yasnippet)))
    (lsp-python-enable)
    (+python/python-setup-shell))
  :hook (python-mode . +python/lsp-python-config)
  :config
  (when (featurep 'dap-mode)
    (require 'dap-python))

  ;; https://github.com/emacs-lsp/lsp-mode/issues/377
  (defun org-babel-edit-prep:python (babel-info)
    "Prepare the local buffer environment for Org source block."
    (let ((lsp-file (or (->> babel-info caddr (alist-get :file))
                        buffer-file-name)))
      (setq-local buffer-file-name lsp-file)
      (setq-local lsp-buffer-uri (lsp--path-to-uri buffer-file-name))
      (+python/lsp-python-config)))
  (defun org-babel-edit-prep:ipython (babel-info)
    "Prepare the local buffer environment for Org source block."
    (let ((lsp-file (or (->> babel-info caddr (alist-get :file))
                        buffer-file-name)))
      (setq-local buffer-file-name lsp-file)
      (setq-local lsp-buffer-uri (lsp--path-to-uri buffer-file-name))
      (+python/lsp-python-config))))

;; Extra Keybindings
(with-eval-after-load 'python
  (+funcs/try-general-major-key python-mode-map
                                "'"  '(+python/repl :which-key "repl")
                                "d"  '(nil :which-key "debug")
                                "db" '(dap-breakpoint-toggle :which-key "breakpoint")
                                "dB" '(dap-breakpoint-condition :which-key "breakpoint-condition")
                                "dr" '(dap-debug :which-key "debug")
                                "c"  '(nil :which-key "compile-exec")
                                "cc" '(+python/python-execute-file :which-key "execute-file")
                                "cC" '(+python/python-execute-file-focus :which-key "execute-file-focus")
                                "f"  '(lsp-format-buffer :which-key "format")
                                "g"  '(nil :which-key "go")
                                "gd" '(lsp-ui-peek-find-definitions :which-key "find-definitions")
                                "gr" '(lsp-ui-peek-find-references :which-key "find-references")
                                "R" '(lsp-rename :which-key "rename")))

(defun +python/pyenv-executable-find (command)
  (executable-find command))

(defun +python/python-setup-shell (&rest args)
  (if (+python/pyenv-executable-find "ipython")
      (progn (setq python-shell-interpreter "ipython")
             (if (version< (replace-regexp-in-string "[\r\n|\n]$" "" (shell-command-to-string "ipython --version")) "5")
                 (setq python-shell-interpreter-args "-i")
               (setq python-shell-interpreter-args "--simple-prompt -i")))
    (progn
      (setq python-shell-interpreter-args "-i")
      (setq python-shell-interpreter "python"))))

(defun +python/python-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond ((+python/pyenv-executable-find "trepan3k") "import trepan.api; trepan.api.debug()")
                     ((+python/pyenv-executable-find "wdb") "import wdb; wdb.set_trace()")
                     ((+python/pyenv-executable-find "ipdb") "import ipdb; ipdb.set_trace()")
                     ((+python/pyenv-executable-find "pudb") "import pudb; pudb.set_trace()")
                     ((+python/pyenv-executable-find "ipdb3") "import ipdb; ipdb.set_trace()")
                     ((+python/pyenv-executable-find "pudb3") "import pudb; pudb.set_trace()")
                     (t "import pdb; pdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (insert "\n")
        (python-indent-line)))))

;;;###autoload
(defun +python/repl ()
  "Open the Python REPL."
  (interactive)
  ;; (process-buffer (run-python nil t t))
  (pop-to-buffer (process-buffer (python-shell-get-or-create-process)))
  (evil-insert-state))

(defun +python/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "%s %s"
                                 (+python/pyenv-executable-find python-shell-interpreter)
                                 (file-name-nondirectory buffer-file-name))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        ;; python-shell--interpreter default value is void...
        ;; Fix inferior-python-mode initialization error
        (setq python-shell--interpreter nil)
        (setq python-shell--interpreter-args nil)
        (inferior-python-mode)))))

(defun +python/python-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
 'normal state'."
  (interactive "P")
  (+python/python-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer)
  (evil-normal-state))


(provide 'init-python)

;;; init-python.el ends here
