;; init-lang-moonbit.el --- MoonBit Configuration 	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  MoonBit Configuration
;;

;;; Code:

(use-package moonbit-mode
  :vc (:url "https://github.com/zsxh/moonbit-mode.git" :branch "feat/emacs-30")
  ;; :vc (:url "https://github.com/cxa/moonbit-mode.git")
  ;; :load-path "~/workspace/emacs/moonbit-mode"
  ;; :mode ("\\.mbt\\'" . moonbit-mode)
  :init
  (defun moonbit-setup ()
    (setq-local tab-width 2)
    (setq-local face-remapping-alist
                '((eglot-semantic-async
                   :weight normal
                   :slant italic
                   :underline t))))
  :hook ((moonbit-mode . eglot-ensure)
         (moonbit-mode . moonbit-setup))
  :config
  ;; (add-to-list
  ;;  'treesit-language-source-alist
  ;;  '(moonbit "https://github.com/moonbitlang/tree-sitter-moonbit.git"))
  (unless (treesit-language-available-p 'moonbit)
    (treesit-install-language-grammar 'moonbit))

  (+eglot/set-leader-keys moonbit-mode-map))

(use-package eglot-moonbit
  :vc (:url "https://github.com/zsxh/eglot-moonbit")
  :after eglot
  :config
  (push '(moonbit-mode . (eglot-moonbit-server . ("moonbit-lsp" "--stdio")))
        eglot-server-programs))

(with-eval-after-load 'nerd-icons
  (add-to-list 'nerd-icons-mode-icon-alist
               '(moonbit-mode nerd-icons-mdicon "nf-md-rabbit_variant" :face nerd-icons-maroon))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("mbt" nerd-icons-mdicon "nf-md-rabbit_variant" :face nerd-icons-maroon)))

(provide 'init-lang-moonbit)

;;; init-lang-moonbit.el ends here
