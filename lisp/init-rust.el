;; init-rust.el --- Rust Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Rust Configurations
;;

;;; Code:

(require 'init-language-server)

;; require `rls' https://github.com/rust-lang/rls
;; >$ rustup component add rls-preview rust-analysis rust-src
(use-package rust-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  :hook (rust-mode . lsp)
  :config
  (setq rust-indent-offset 2))

(use-package cargo
  :ensure t
  :after rust-mode)

(with-eval-after-load 'rust-mode
  (+language-server/set-common-leader-keys rust-mode-map)
  (+funcs/major-mode-leader-keys
   rust-mode-map
   "c" '(nil :which-key "cargo")
   "c." '(cargo-process-repeat :which-key "repeat-last-command")
   "cC" '(cargo-process-clean :which-key "clean")
   "cX" '(cargo-process-run-example :which-key "run-example")
   "cc" '(cargo-process-build :which-key "build")
   "cd" '(cargo-process-doc :which-key "generate-doc")
   "cD" '(cargo-process-doc-open :which-key "open-doc")
   "ce" '(cargo-process-bench :which-key "benchmark")
   "cf" '(cargo-process-fmt :which-key "fmt")
   "ci" '(cargo-process-init :which-key "init")
   "cl" '(cargo-process-clippy :which-key "clippy")
   "cn" '(cargo-process-new :which-key "new")
   "co" '(cargo-process-current-file-tests :which-key "current-file-unit-test")
   "cs" '(cargo-process-search :which-key "search")
   "ct" '(cargo-process-current-test :which-key "current-unit-test")
   "cu" '(cargo-process-update :which-key "update-dependencies")
   "cx" '(cargo-process-run :which-key "run")
   "cv" '(cargo-process-check :which-key "check")
   "cT" '(cargo-process-test :which-key "all-unit-tests")))


(provide 'init-rust)

;;; init-rust.el ends here
