;; init-java.el --- Java Configurations	-*- lexical-binding: t -*-

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
;;  Java Configurations
;;

;;; Code:

;; lsp-java
(use-package lsp-java
  :ensure t
  :requires init-lsp
  :commands lsp-java-enable
  :preface
  (defun +java/lsp-java-configs ()
    (setq-local company-minimum-prefix-length 0)
    (setq-local company-lsp-cache-candidates t)
    (setq-local company-backends
                '((company-lsp :separate company-yasnippet)))
    ;; FIXME: lsp workspace doesnot work well
    ;; (setq lsp-java--workspace-folders (list (projectile-project-root)))
    (lsp-java-enable)
    ;; (lsp-workspace-folders-add (list (lsp-java--get-root)))
    )
  :hook (java-mode . +java/lsp-java-configs)
  :config
  (setq lsp-java-server-install-dir "~/.emacs.d/.cache/eclipse.jdt.ls/server")
  ;; debugger
  (when (featurep 'dap-mode)
    (require 'dap-java))
  (+funcs/try-general-major-key java-mode-map
                                "d"  '(nil :which-key "debug")
                                "db" '(dap-breakpoint-toggle :which-key "breakpoint")
                                "dB" '(dap-breakpoint-condition :which-key "breakpoint-condition")
                                "dr" '(dap-java-debug :which-key "debug")
                                "dt" '(dap-java-debug-test-method :which-key "debug-junit-test-method")
                                "dT" '(dap-java-debug-test-class :which-key "debug-junit-class")
                                "dk" '(+dap/debug-key-settings--toggle :which-key "toggle-debug-keybindings")
                                "f"  '(lsp-format-buffer :which-key "format")
                                "g"  '(nil :which-key "go")
                                "gd" '(lsp-ui-peek-find-definitions :which-key "find-definitions")
                                "gi" '(lsp-goto-implementation :which-key "find-implementation")
                                "gr" '(lsp-ui-peek-find-references :which-key "find-references")
                                "i"  '(nil :which-key "implement")
                                "ic" '(lsp-java-add-import :which-key "import-class")
                                "im" '(lsp-java-add-unimplemented-methods :which-key "add-unimplemented-methods")
                                "j"  '(+java/set-jdk :which-key "set-jdk")
                                "r"  '(nil :which-key "run")
                                "rt" '(dap-java-run-test-method :which-key "run-junit-test-method")
                                "rT" '(dap-java-run-test-class :which-key "run-junit-class")
                                "R"  '(lsp-rename :which-key "rename")))

(defvar jdk-installed-dir "/usr/local/"
  "JDK isntalled directory.")

;;;###autoload
(defun +java/set-jdk (name)
  "Set JDK version as NAME."
  (interactive (list (completing-read "JDK-version: " (+java/java-version-list))))
  (let ((target (expand-file-name name jdk-installed-dir))
        (link-name (expand-file-name "jdk" jdk-installed-dir)))
    (+funcs/sudo-shell-command (concat "ln -nsf " target " " link-name))))

(defun +java/java-version-list ()
  "Return all jdks in list."
  (let ((files (directory-files jdk-installed-dir))
        (result nil))
    (dolist (file files)
      (if (string-match "jdk-" file)
          (push file result)))
    result))


(provide 'init-java)

;;; init-java.el ends here
