;; init-lang-clojure.el --- Clojure Lang Config	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Clojure Lang Config
;;

;;; Code:

;; Install Leiningen: https://github.com/technomancy/leiningen#installation
;; Install cider-nrepl: https://docs.cider.mx/cider-nrepl/usage.html#_via_leiningen
;; Install shadow-cljs: https://shadow-cljs.org/
;; Install clojure lsp server:
;; 1) `lsp-install-server' lsp-mode auto install server
;; 2) https://github.com/snoe/clojure-lsp#installation and set `lsp-clojure-server-store-path'
;; clojurescript: https://clojurescript.org/

(use-package clojure-mode
  :mode (("\\.\\(clj\\|dtm\\|edn\\)\\'" . clojure-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :hook ((clojure-mode clojurec-mode clojurescript-mode) . +clojure/lsp)
  :config
  (require 'lsp-clojure)
  (dolist (mode-map '(clojure-mode-map clojurec-mode-map clojurescript-mode-map))
    (+language-server/set-common-leader-keys (symbol-value mode-map)))
  (defun +clojure/lsp ()
    (if (and buffer-file-name
             (member (file-name-extension buffer-file-name) '("clj" "cljs" "cljc")))
        (lsp-deferred))))

;; https://cider.mx/
(use-package cider
  :defer t)

;; TODO: clojure debugger
;; https://github.com/clojure-emacs/sayid
(use-package sayid
  :hook (clojure-mode . sayid-setup-package))


(provide 'init-lang-clojure)

;;; init-lang-clojure.el ends here
