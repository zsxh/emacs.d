;; init-lang-clojure.el --- Clojure Lang Config	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Clojure Lang Config
;;

;;; Code:

;; https://github.com/snoe/clojure-lsp#installation
(use-package clojure-mode
  :mode (("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :hook ((clojure-mode clojurec-mode clojurescript-mode) . lsp)
  :config
  (require 'lsp-clojure)
  (dolist (mode-map '(clojure-mode-map clojurec-mode-map clojurescript-mode-map))
    (+language-server/set-common-leader-keys (symbol-value mode-map))))

;; TODO: cider configs and learn some shadow-cljs
(use-package cider
  :defer t)


(provide 'init-lang-clojure)

;;; init-lang-clojure.el ends here
