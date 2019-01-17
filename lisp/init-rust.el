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
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(with-eval-after-load 'rust-mode
  (setq rust-indent-offset 2)

  (defun +rust/cargo-command (command)
    (let ((default-directory (projectile-project-root))
          (compile-command command))
      (compile compile-command)))

  (defun +rust/cargo-run ()
    (interactive)
    (+rust/cargo-command "cargo run"))

  (defun +rust/cargo-test ()
    (interactive)
    (+rust/cargo-command "cargo test"))

  (defun +rust/cargo-check ()
    (interactive)
    (+rust/cargo-command "cargo check"))

  (+funcs/set-leader-keys-for-major-mode
   'rust-mode-map
   "c" '(nil :which-key "cargo")
   "cc" '(+rust/cargo-check :which-key "check")
   "cr" '(+rust/cargo-run :which-key "run")
   "ct" '(+rust/cargo-test :which-key "test")))

(add-hook 'rust-mode-hook 'lsp)


(provide 'init-rust)

;;; init-rust.el ends here
