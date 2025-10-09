;; init-file-modes.el --- file modes	-*- lexical-binding: t -*-
;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  File Modes
;;

;;; Code:

(when (version< emacs-version "31")
  (when (treesit-ready-p 'toml)
    (add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode)))
  (when (treesit-ready-p 'json)
    (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))))

;; Nix
(use-package nix-ts-mode
  :if (treesit-ready-p 'nix)
  :mode "\\.nix\\'"
  :hook ((nix-ts-mode . eglot-ensure))
  :config
  (require 'eglot)
  (push `(nix-ts-mode . ,(eglot-alternatives '("nil" "rnix-lsp" "nixd"))) eglot-server-programs)
  (+eglot/set-leader-keys nix-ts-mode-map)
  (cl-defmethod +eglot/workspace-configuration (server &context (major-mode nix-ts-mode))
    '(:nixd
      (:nixpkgs (:expr "import <nixpkgs> { }")
       :formatting (:command ["nixfmt"])))))

;; Lua
(use-package lua-ts-mode
  :if (treesit-ready-p 'lua)
  :mode "\\.lua\\'"
  :hook (lua-ts-mode . eglot-ensure)
  :config
  (+eglot/set-leader-keys lua-ts-mode-map))

;; Markdowm
(use-package markdown-mode
  :defer t
  :config
  (setq-default markdown-fontify-code-blocks-natively t)
  (setopt markdown-header-scaling t)
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
(if (treesit-ready-p 'yaml)
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
  (use-package yaml-mode
    :defer t))

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

;; PDF, EPUB, MOBI, FB2, XPS/OpenXPS, CBZ
;; The Emacs Reader (via MuPDF)
(use-package reader
  :vc (:url "https://codeberg.org/divyaranjan/emacs-reader" :make "all")
  :bind ((:map reader-mode-map
          ("j" . reader-scroll-down-or-next-page)
          ("k" . reader-scroll-up-or-prev-page)
          ("h" . reader-scroll-left)
          ("l" . reader-scroll-right)
          ("d" . reader-next-page)
          ("u" . reader-previous-page)
          ("P" . reader-goto-page)
          ("H" . reader-fit-to-height)
          ("W" . reader-fit-to-width)
          ("q" . nil)))
  :config
  (with-eval-after-load 'evil
    (evil-define-key '(normal motion visual)
      reader-outline-mode-map
      "q" 'quit-window)
    (evil-define-key '(normal motion visual)
      reader-mode-map
      "g" nil
      "gg" 'reader-first-page
      "G" 'reader-last-page
      "P" 'reader-goto-page))

  (with-eval-after-load 'doom-modeline
    (doom-modeline-def-segment reader-pages
      "Display Reader pages."
      (format "  P%d/%d "
              (or (eval `(reader-current-doc-pagenumber)) 0)
              reader-current-doc-pagecount))

    (doom-modeline-def-modeline 'reader
      '(bar window-number modals matches buffer-info reader-pages)
      '(compilation misc-info major-mode process vcs time))

    (add-to-list 'doom-modeline-mode-alist '(reader-mode . reader))))

;; google protobuf languages
(use-package protobuf-mode
  :defer t)

;; [just](https://github.com/casey/just) is a handy way to save and run project-specific commands.
(use-package just-ts-mode
  :if (treesit-ready-p 'just)
  :defer t)

;; SQL
;; use the SQL indent support features of sql-indent.
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;; `Hurl' is a command line tool that runs HTTP requests defined in a simple plain text format.
(use-package hurl-mode
  :vc (:url "https://github.com/JasZhe/hurl-mode")
  :bind ((:map hurl-response-mode-map
          ("C-j" . outline-next-heading)
          ("C-k" . outline-previous-heading)
          ("TAB" . outline-toggle-children)
          ("S-TAB" . outline-cycle-children)))
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'hurl-response-mode 'normal)
    (evil-define-key '(normal visual) hurl-response-mode-map
      "q" #'quit-window))
  (+funcs/major-mode-leader-keys
   hurl-mode-map
   "t" '(hurl-mode-test-request-single :which-key "test")
   "T" '(hurl-mode-test-request-file :which-key "test-all")
   "x" '(hurl-mode-send-request-single :which-key "request")
   "X" '(hurl-mode-send-request-file :which-key "request-all"))
  (+funcs/major-mode-leader-keys
   hurl-response-mode-map
   "n" '(outline-next-heading :which-key "outline-next-heading")
   "p" '(outline-previous-heading :which-key "outline-previous-heading")))

;; GRPClient
;; TODO: `grpclient-mode-map' key bindings
(use-package grpclient
  :if (executable-find "grpcurl")
  :vc (:url "https://github.com/Prikaz98/grpclient.el.git")
  :mode ("\\.grpc\\'" . grpclient-mode)
  :defer t)

;; Mermaid
(use-package mermaid-mode
  :defer t)


(provide 'init-file-modes)

;;; init-file-modes.el ends here
