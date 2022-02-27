;; init-lang-clojure.el --- Clojure Lang Config	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Clojure Lang Config
;;

;;; Code:

;; Install clj: https://clojure.org/guides/getting_started#_installation_on_linux
;; Install cider-nrepl: https://docs.cider.mx/cider-nrepl/usage.html#_via_leiningen
;; Install shadow-cljs: https://shadow-cljs.org/
;; Install clojure lsp server:
;; 1) `lsp-install-server' lsp-mode auto install server
;; 2) https://github.com/snoe/clojure-lsp#installation and set `lsp-clojure-server-store-path'
;;
;; clojurescript: https://clojurescript.org/
;; shadow-cljs repl connect js runtime: https://shadow-cljs.github.io/docs/UsersGuide.html#repl-troubleshooting

(use-package clojure-mode
  :mode (("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :hook ((clojure-mode clojurec-mode clojurescript-mode) . +clojure/lsp)
  :config
  (setq clojure-toplevel-inside-comment-form t)
  (require 'lsp-clojure)
  (dolist (mode-map '(clojure-mode-map clojurec-mode-map clojurescript-mode-map))
    (+language-server/set-common-leader-keys (symbol-value mode-map)))
  (defun +clojure/lsp ()
    (when (and buffer-file-name
               (member (file-name-extension buffer-file-name) '("clj" "cljs" "cljc")))
      ;; (setq-local lsp-completion-no-cache t)
      (lsp-deferred))))

;; https://cider.mx/
;; `cider-insert-last-sexp-in-repl'
(use-package cider
  :defer t)

(use-package lsp-clojure
  :ensure lsp-mode
  :defer t
  :custom
  (lsp-clojure-workspace-dir (expand-file-name (locate-user-emacs-file "cache/clojure-workspace/"))))

;; TODO: clojurescript debugger
;; https://emacs-lsp.github.io/lsp-mode/tutorials/debugging-clojure-script/

(use-package sayid
  :after cider
  :config
  ;; FIXME: temporary fixed clojure-cli dependencies
  (add-to-list 'cider-jack-in-dependencies `("com.billpiel/sayid" ,sayid-injected-plugin-version)))


(provide 'init-lang-clojure)

;;; init-lang-clojure.el ends here
