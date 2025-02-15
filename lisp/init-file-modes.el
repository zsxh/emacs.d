;; init-file-modes.el --- file modes	-*- lexical-binding: t -*-
;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  File Modes
;;

;;; Code:

;; Nix
(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook ((nix-ts-mode . eglot-ensure))
  :config
  (require 'eglot)
  (unless (eglot--lookup-mode 'nix-ts-mode)
    (push `(nix-ts-mode . ,(eglot-alternatives '("nil" "rnix-lsp" "nixd"))) eglot-server-programs))
  (+eglot/set-leader-keys nix-ts-mode-map)
  (cl-defmethod +eglot/workspace-configuration (server &context (major-mode nix-ts-mode))
    '(:nixd
      (:nixpkgs (:expr "import <nixpkgs> { }")
       :formatting (:command ["alejandra"])))))

;; Lua
(use-package lua-ts-mode
  :hook (lua-ts-mode . eglot-ensure)
  :config
  (+eglot/set-leader-keys lua-ts-mode-map))

;; Markdowm
(use-package markdown-mode
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (+funcs/major-mode-leader-keys
   markdown-mode-map
   "'" '(markdown-edit-code-block :which-key "edit-code-block")
   "T" '(nil :which-key "toggle")
   "Ti" '(markdown-toggle-inline-images :which-key "inline-images")))

;; CSV
(use-package csv-mode :defer t)
(use-package rainbow-csv-mode
  :vc (:url "https://github.com/emacs-vs/rainbow-csv")
  :hook ((csv-mode tsv-mode) . rainbow-csv-mode))

;; Yaml
(use-package yaml-mode
  :defer t)

(use-package yaml-ts-mode
  :ensure nil
  :defer t
  :config
  (with-eval-after-load 'eglot
    (when-let* ((_ (file-exists-p schemastore-file))
                (schemas (with-temp-buffer
                           (insert-file-contents schemastore-file)
                           (jsonrpc--json-read))))
      (cl-defmethod +eglot/workspace-configuration (server &context (major-mode yaml-ts-mode))
        `(:yaml
          (:schemas ,schemas))))))

;; Json
(use-package json-ts-mode
  :ensure nil
  :defer t
  :config
  (with-eval-after-load 'eglot
    (when-let* ((_ (file-exists-p schemastore-file))
                (schemas (with-temp-buffer
                           (insert-file-contents schemastore-file)
                           (jsonrpc--json-read))))
      ;; FIXME: `eglot--workspace-configuration-plist' set wrong major mode
      (cl-defmethod +eglot/workspace-configuration (server &context (major-mode js-json-mode))
        `(:json
          (:validate (:enable t)
           :schemas ,schemas)))))
  (+funcs/major-mode-leader-keys json-ts-mode-map
                                 "j" '(counsel-jq :which-key "counsel-jq")
                                 "p" '(json-pretty-print-buffer :which-key "pretty-print")))

;; PDF
;; https://github.com/vedang/pdf-tools
;; install `poppler'
(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (with-eval-after-load 'evil-collection
    (evil-collection-pdf-setup)
    (evil-define-key 'normal pdf-view-mode-map
      (kbd "C-s") 'isearch-forward
      (kbd "C-r") 'isearch-backward
      "d" 'pdf-view-scroll-up-or-next-page
      "u" 'pdf-view-scroll-down-or-previous-page)))

(use-package pdf-view-restore
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename (locate-user-emacs-file "cache/.pdf-view-restore")))

;; LaTeX
;;
;; TODO: https://github.com/dandavison/xenops
;; NOTE: https://karthinks.com/software/latex-input-for-impatient-scholars/
;; NOTE: karthinks Fast, Async LaTeX Previews https://www.youtube.com/watch?v=n-AfvuV-bYo

;; google protobuf languages
(use-package protobuf-mode
  :defer t)

;; [just](https://github.com/casey/just) is a handy way to save and run project-specific commands.
(use-package just-mode
  :defer t)

;; SQL
;; use the SQL indent support features of sql-indent.
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))


(provide 'init-file-modes)

;;; init-file-modes.el ends here
