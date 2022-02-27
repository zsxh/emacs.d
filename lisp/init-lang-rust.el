;; init-lang-rust.el --- Rust Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Rust Configurations
;;

;;; Code:

(require 'init-lsp)

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . lsp-deferred)
  :config
  (setq rust-indent-offset 2)
  (require 'lsp-rust))

;; Install rust-analyzer:
;;    1) lsp-mode auto install out-of-box(`lsp-package-ensure')
;; OR 2) use `lsp-install-server' install server
;; OR 3) download binary executable file from https://github.com/rust-analyzer/rust-analyzer/releases and set `lsp-rust-analyzer-store-path'
;; OR 4) build from source code
;; $ git clone git@github.com:rust-analyzer/rust-analyzer && cd rust-analyzer
;; $ cargo xtask install --server
;; $ rustup component add rust-src
(use-package lsp-rust
  :defer t
  :ensure lsp-mode
  :custom
  ;; (lsp-rust-analyzer-download-url "https://github.com/rust-analyzer/rust-analyzer/releases/download/nightly/rust-analyzer-linux")
  (lsp-rust-analyzer-server-display-inlay-hints t))

;; TODO: debugger, (require 'dap-cpptools), M-x `dap-cpptools-setup'
;; https://emacs-lsp.github.io/dap-mode/page/configuration/#vscode-cpptools

(use-package dap-gdb-lldb
  :after lsp-rust
  :ensure dap-mode)

(use-package cargo
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


(provide 'init-lang-rust)

;;; init-lang-rust.el ends here
