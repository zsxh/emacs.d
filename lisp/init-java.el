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

(require 'init-language-server)

(use-package lsp-java
  :defer t
  :quelpa ((lsp-java :fetcher github :repo "emacs-lsp/lsp-java"))
  :init
  (setq lsp-java-server-install-dir "~/.emacs.d/.cache/eclipse.jdt.ls/server"))

(use-package dap-java :after (lsp-java))

(defun +java/lsp-java-config ()
  (require 'lsp-java)
  (setq-local company-minimum-prefix-length 0)
  ;; (setq-local company-lsp-cache-candidates t)
  (+java/set-leader-keys)
  (lsp)
  ;; (setq-local company-backends
  ;;             '((company-lsp :separate company-yasnippet)))
  )

(add-hook 'java-mode-hook '+java/lsp-java-config)

(defun +java/set-leader-keys ()
  (+funcs/set-leader-keys-for-major-mode
   'java-mode-map
   "d" '(nil :which-key "debug")
   "db" '(dap-breakpoint-toggle :which-key "breakpoint")
   "dB" '(dap-breakpoint-condition :which-key "breakpoint-condition")
   "dr" '(dap-java-debug :which-key "debug")
   "dt" '(dap-java-debug-test-method :which-key "debug-junit-test-method")
   "dT" '(dap-java-debug-test-class :which-key "debug-junit-class")
   "dk" '(+dap/debug-key-settings--toggle :which-key "toggle-debug-keybindings")
   "i" '(nil :which-key "implement")
   "ic" '(lsp-java-add-import :which-key "import-class")
   "im" '(lsp-java-add-unimplemented-methods :which-key "add-unimplemented-methods")
   "j" '(+java/set-jdk :which-key "set-jdk")
   "r" '(nil :which-key "run")
   "rt" '(dap-java-run-test-method :which-key "run-junit-test-method")
   "rT" '(dap-java-run-test-class :which-key "run-junit-class")))


(defvar jdks-installed-dir "/usr/local/"
  "JDKs intalled directory.")

;;;###autoload
(defun +java/set-jdk (jdk-version)
  "Select JDK-VERSION in `jdks-installed-dir' as global version,
JDK-VERSION directory name prefix `jdk-' is required,
`jdks-installed-dir'/jdk/bin in $PATH is required."
  (interactive (list (completing-read "JDK-version: " (+java/java-version-list))))
  (let ((target (expand-file-name jdk-version jdks-installed-dir))
        (link-name (expand-file-name "jdk" jdks-installed-dir)))
    (+funcs/sudo-shell-command (concat "ln -nsf " target " " link-name))))

(defun +java/java-version-list ()
  "Find all jdks which start with `jdk-' in `jdks-installed-dir'"
  (let ((files (directory-files jdks-installed-dir))
        (result nil))
    (dolist (file files)
      (if (string-match "jdk-" file)
          (push file result)))
    result))


(provide 'init-java)

;;; init-java.el ends here
