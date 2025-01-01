;; init-lang-rust.el --- Rust Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Rust Configurations
;;

;;; Code:

;; NOTE: Install `rust-analyzer'
;; https://github.com/rust-analyzer/rust-analyzer/releases

(use-package rust-mode
  :defer t
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . eglot-ensure)
  :config
  (setq rust-indent-offset 2))

(use-package rust-ts-mode
  :ensure nil
  :defer t
  :hook (rust-ts-mode . eglot-ensure)
  :config
  (setq rust-ts-mode-indent-offset 2))

(add-hook-run-once 'rust-mode-hook #'+rust/set-custom-leader-keys)
(add-hook-run-once 'rust-ts-mode-hook #'+rust/set-custom-leader-keys)

;; TODO: rust-analyzer :experimental/externalDocs
;; https://github.com/joaotavora/eglot/discussions/1327
(defun +rust/eglot-open-external-documentation ()
  "Open a URL to the documentation for the symbol under point."
  (interactive)
  (eglot-server-capable-or-lose :experimental :externalDocs)
  (let ((res (jsonrpc-request (eglot--current-server-or-lose)
                              :experimental/externalDocs
                              (eglot--TextDocumentPositionParams))))
    (when res
      (let ((local (plist-get res :local))
            (web (or (plist-get res :web)
                     res)))
        (if (and local (file-exists-p (eglot-uri-to-path local)))
            (browse-url local)
          (browse-url web))))))

(use-package cargo
  :defer t)

(defun +rust/set-custom-leader-keys (&optional map)
  (let ((mode-map (or map (keymap-symbol (current-local-map)))))
    (require 'cargo nil t)
    (+eglot/set-leader-keys mode-map)
    (+funcs/major-mode-leader-keys
     mode-map
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
     "cT" '(cargo-process-test :which-key "all-unit-tests"))))


(provide 'init-lang-rust)

;;; init-lang-rust.el ends here
