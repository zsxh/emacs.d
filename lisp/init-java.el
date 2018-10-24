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
    (setq-local company-backends
                '((company-lsp :separate company-yasnippet)))
    ;; FIXME: lsp workspace doesnot work well
    (setq lsp-java--workspace-folders (list (projectile-project-root)))
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
                                "gr" '(lsp-ui-peek-find-references :which-key "find-references")
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

;; meghanada
;; (use-package meghanada
;;   :ensure t
;;   :commands meghanada-mode
;;   :hook (java-mode . (lambda ()
;;                        (google-set-c-style)
;;                        (google-make-newline-indent)
;;                        (meghanada-mode t)
;;                        ;; (smartparens-mode t)
;;                        (rainbow-delimiters-mode t)
;;                        ;; (highlight-symbol-mode t)
;;                        ;; use code format
;;                        ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
;;                        ))
;;   :bind (:map meghanada-mode-map
;;               ("C-S-t" . meghanada-switch-testcase)
;;               ("M-RET" . meghanada-local-variable)
;;               ("C-M-." . helm-imenu)
;;               ("M-r" . meghanada-reference)
;;               ("M-t" . meghanada-typeinfo)
;;               ("C-z" . hydra-meghanada/body))
;;   :config
;;   (use-package realgud
;;     :ensure t
;;     :defer t)
;;   (setq indent-tabs-mode nil)
;;   (setq tab-width 2)
;;   (setq c-basic-offset 2)
;;   (setq meghanada-server-remote-debug t)
;;   (setq meghanada-javac-xlint "-Xlint:all,-processing")
;;   (setq meghanada-server-install-dir (locate-user-emacs-file ".cache/meghanada/"))

;;   (+funcs/try-general-major-key java-mode-map
;;                                 "h"  '(hydra-meghanada/body :which-key "help")
;;                                 "c"  '(nil :which-key "compile")
;;                                 "cf" '(meghanada-compile-file :which-key "compile-file")
;;                                 "cp" '(meghanada-compile-project :which-key "compile-project")
;;                                 "r"  '(nil :which-key "run")
;;                                 "rm" '(meghanada-exec-main :which-key "exec-main")
;;                                 "g"  '(nil :which-key "goto")
;;                                 "gd" '(meghanada-jump-declaration :which-key "jump-declaration")
;;                                 "gs" '(meghanada-jump-symbol :which-key "find-class")
;;                                 "j"  '(+java/set-jdk :which-key "set-jdk")))

;; (defhydra hydra-meghanada (:hint nil :exit t)
;;   "
;; ^Edit^                           ^Tast or Task^
;; ^^^^^^-------------------------------------------------------
;; _f_: meghanada-compile-file      _m_: meghanada-restart
;; _c_: meghanada-compile-project   _t_: meghanada-run-task
;; _o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
;; _s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
;; _v_: meghanada-local-variable    _R_: meghanada-run-junit-recent
;; _i_: meghanada-import-all        _r_: meghanada-reference
;; _g_: magit-status                _T_: meghanada-typeinfo
;; _l_: helm-ls-git-ls
;; _q_: exit
;; "
;;   ("f" meghanada-compile-file)
;;   ("m" meghanada-restart)

;;   ("c" meghanada-compile-project)
;;   ("o" meghanada-optimize-import)
;;   ("s" meghanada-switch-test-case)
;;   ("v" meghanada-local-variable)
;;   ("i" meghanada-import-all)

;;   ("g" magit-status)
;;   ("l" helm-ls-git-ls)

;;   ("t" meghanada-run-task)
;;   ("T" meghanada-typeinfo)
;;   ("j" meghanada-run-junit-test-case)
;;   ("J" meghanada-run-junit-class)
;;   ("R" meghanada-run-junit-recent)
;;   ("r" meghanada-reference)

;;   ("q" exit)
;;   ("z" nil "leave"))


(provide 'init-java)

;;; init-java.el ends here
