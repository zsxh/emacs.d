;; init-lang-moonbit.el --- MoonBit Configuration 	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  MoonBit Configuration
;;

;;; Code:

(use-package moonbit-ts-mode
  ;; :init
  ;; (defun moonbit-setup ()
  ;;   (setq-local tab-width 2)
  ;;   (setq-local face-remapping-alist
  ;;               '((eglot-semantic-async
  ;;                  :weight normal
  ;;                  :slant italic
  ;;                  :underline t))))
  :hook ((moonbit-mode . eglot-ensure)
         ;; (moonbit-mode . moonbit-setup)
         )
  :config
  (add-to-list 'treesit-language-source-alist
               '(moonbit "https://github.com/moonbitlang/tree-sitter-moonbit.git" "main" "src"))
  (add-to-list 'treesit-language-source-alist
               '(moonbit_mbtp "https://github.com/moonbitlang/tree-sitter-moonbit.git"
                              "main" "grammars/mbtp/src"))
  (unless (treesit-language-available-p 'moonbit)
    (treesit-install-language-grammar 'moonbit))
  (unless (treesit-language-available-p 'moonbit_mbtp)
    (treesit-install-language-grammar 'moonbit_mbtp))
  (+eglot/set-leader-keys moonbit-ts-mode-map))

(use-package eglot-moonbit
  :vc (:url "https://github.com/zsxh/eglot-moonbit")
  :after eglot
  :config
  ;; (push '(moonbit-ts-mode . (eglot-moonbit-server . ("moonbit-lsp" "--stdio")))
  ;;       eglot-server-programs)
  (push '((moonbit-ts-mode :language-id "moonbit")
          . (eglot-moonbit-server . ("moonbit" "lsp")))
        eglot-server-programs))

(with-eval-after-load 'nerd-icons
  (add-to-list 'nerd-icons-mode-icon-alist
               '(moonbit-ts-mode nerd-icons-mdicon "nf-md-rabbit_variant" :face nerd-icons-maroon))
  (add-to-list 'nerd-icons-extension-icon-alist
               '("mbt" nerd-icons-mdicon "nf-md-rabbit_variant" :face nerd-icons-maroon)))

(provide 'init-lang-moonbit)

;;; init-lang-moonbit.el ends here
