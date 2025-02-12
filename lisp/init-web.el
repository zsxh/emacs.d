;; init-web.el --- Web Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Web Configurations
;;

;;; Code:

;; NOTE: install HTML/CSS/JSON/ESLint language servers `vscode-langservers-extracted'

(use-package html-ts-mode
  :defer t
  ;; :hook (html-ts-mode . eglot-ensure)
  :config
  (add-hook-run-once 'html-ts-mode-hook '+eglot/set-leader-keys))

;; Web mode for html,xml...
(use-package web-mode
  :commands web-mode
  :init
  (define-derived-mode xml-web-mode web-mode "XML"
    "A major mode derived from web-mode, for editing pom.xml files.")
  ;; TODO: use `html-ts-mode' instead of `html-web-mode'
  (define-derived-mode html-web-mode web-mode "HTML"
    "A major mode derived from web-mode, for editing html files.")
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ;; ("\\.html?\\'" . web-mode)
         ("\\.html?\\'" . html-web-mode)
         ("\\.xml?\\'" . xml-web-mode))
  :hook ((web-mode . +web/config)
         (html-web-mode . eglot-ensure))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-sql-indent-offset 2)

  (defun +web/config ()
    (setq-local company-backends
                '(company-capf company-files company-css company-dabbrev)))

  (add-to-list 'hs-special-modes-alist
               '(xml-web-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 sgml-skip-tag-forward
                 nil))

  (+funcs/major-mode-leader-keys
   web-mode-map
   "f" '(web-mode-buffer-indent :which-key "indent-buffer"))
  (+funcs/major-mode-leader-keys
   html-web-mode-map
   "f" '(web-mode-buffer-indent :which-key "indent-buffer"))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs `(html-web-mode . ,(eglot-alternatives '(("vscode-html-language-server" "--stdio") ("html-languageserver" "--stdio"))))))
  (add-hook-run-once 'html-web-mode-hook '+eglot/set-leader-keys))

(use-package css-mode
  :ensure nil
  :defer t
  :hook (css-base-mode . (lambda ()
                           (setq-local company-backends
                                       '(company-capf company-files company-css company-dabbrev))
                           (eglot-ensure)))
  :config
  (setq css-indent-offset 2)
  (dolist (mode-map '(css-mode-map css-ts-mode-map))
    (+funcs/major-mode-leader-keys
     mode-map
     "c" '(css-cycle-color-format :which-key "css-cycle-color-format")))
  (add-hook-run-once 'css-base-mode-hook '+eglot/set-leader-keys))

;; edit xml
(use-package nxml-mode
  :ensure nil
  :defer t
  :config
  (setq nxml-slash-auto-complete-flag t))

;; edit html
(use-package sgml-mode
  :ensure nil
  :hook (html-mode . eglot-ensure)
  :commands (sgml-slash sgml-skip-tag-forward)
  :config
  ;; https://emacs.stackexchange.com/questions/33240/html-mode-that-closes-tags
  ;; automatic insertion of the closing tag if you type </ or
  ;; pressing C-c / or C-c C-e or C-c / inserts a closing tag (the whole </foo>).
  (setq sgml-quick-keys 'close)
  (add-hook-run-once 'html-mode-hook '+eglot/set-leader-keys)
  (define-advice sgml-slash (:override (arg) advice)
    "Insert ARG slash characters.
Behaves electrically if `sgml-quick-keys' is non-nil."
    (interactive "p")
    (cond
     ((not (and (eq (char-before) ?<) (= arg 1)))
      (sgml-slash-matching arg))
     ((eq sgml-quick-keys 'indent)
      (insert-char ?/ 1)
      (indent-according-to-mode))
     ((eq sgml-quick-keys 'close)
      (delete-char -1)
      ;; save point
      (save-excursion
        (sgml-close-tag)))
     (t
      (insert-char ?/ arg)))))

;; This is a tool to manually explore and test HTTP REST webservices.
;; Runs queries from a plain-text query sheet, displays results as a pretty-printed XML, JSON and even images.
;; https://github.com/pashky/restclient.el
(use-package restclient
  :commands restclient-mode
  :config
  (with-eval-after-load 'evil
    (evil-define-key 'normal restclient-mode-map
      (kbd "RET") 'org-open-at-point)))

;;;###autoload
(defun +web/restclient-new-buffer ()
  "Create a restclient buffer."
  (interactive)
  (let* ((restclient-buffer-name "*restclient*")
         (restclient-buffer (get-buffer restclient-buffer-name)))
    (unless restclient-buffer
      (setq restclient-buffer (generate-new-buffer restclient-buffer-name))
      (with-current-buffer restclient-buffer
        (restclient-mode)
        (when (functionp 'cape-company-to-capf)
          (setq-local completion-at-point-functions (push (cape-company-to-capf 'company-restclient) completion-at-point-functions)))
        (insert "# -*- restclient -*-
# https://github.com/pashky/restclient.el
#
# GET https://api.github.com
# User-Agent: Emacs Restclient
# #
# POST https://jira.atlassian.com/rest/api/2/search
# Content-Type: application/json
# {}
# #
# POST https://somehost/api
# Content-Type: application/x-www-form-urlencoded
# param1=value1&param2=value2\n")))
    (switch-to-buffer restclient-buffer)))

;; This is a elisp library for websocket clients to talk to websocket servers,
;; and for websocket servers to accept connections from websocket clients.
;; This library is designed to be used by other library writers,
;; to write apps that use websockets, and is not useful by itself.
;; https://github.com/ahyatt/emacs-websocket
;; https://blog.abrochard.com/websockets.html
(use-package websocket
  :defer t)

;; plz is an HTTP library for Emacs. It uses curl as a backend, which avoids
;; some of the issues with using Emacs’s built-in url library .
(use-package plz
  :commands (plz))


(provide 'init-web)

;;; init-web.el ends here
