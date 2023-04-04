;; init-web.el --- Web Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Web Configurations
;;

;;; Code:

;; YAML config
(use-package yaml-mode
  :commands yaml-mode
  :config (add-hook 'yaml-mode-hook
                    (lambda ()
                      (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Web mode for html,xml...
(use-package web-mode
  :commands web-mode
  :init
  (define-derived-mode xml-web-mode web-mode "XML"
    "A major mode derived from web-mode, for editing pom.xml files.")
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.xml?\\'" . xml-web-mode))
  :hook ((web-mode . +web/config))
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

  (defun +web/formatter ()
    (interactive)
    (format-all-buffer))

  (+funcs/major-mode-leader-keys
   web-mode-map
   "f" '(+web/formatter :which-key "format-html")))

(use-package css-mode
  :ensure nil
  :defer t
  :hook ((css-mode css-ts-mode) . (lambda ()
                                    (setq-local company-backends
                                                '(company-capf company-files company-css company-dabbrev))))
  :config
  (setq css-indent-offset 2)
  (dolist (mode-map '(css-mode-map css-ts-mode-map))
    (+funcs/major-mode-leader-keys
     mode-map
     "c" '(css-cycle-color-format :which-key "css-cycle-color-format"))))

;; edit xml
(use-package nxml-mode
  :ensure nil
  :defer t
  :config
  (setq nxml-slash-auto-complete-flag t))

;; edit html
(use-package sgml-mode
  :ensure nil
  :commands (sgml-slash sgml-skip-tag-forward)
  :config
  ;; https://emacs.stackexchange.com/questions/33240/html-mode-that-closes-tags
  ;; automatic insertion of the closing tag if you type </ or
  ;; pressing C-c / or C-c C-e or C-c / inserts a closing tag (the whole </foo>).
  (setq sgml-quick-keys 'close))

;; This is a tool to manually explore and test HTTP REST webservices.
;; Runs queries from a plain-text query sheet, displays results as a pretty-printed XML, JSON and even images.
;; https://github.com/pashky/restclient.el
(use-package restclient
  :commands restclient-mode
  :config
  (use-package company-restclient
    :config
    (add-to-list 'company-backends 'company-restclient))
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

;; `verb' is an attempt to improve upon the core idea of the `restclient' package
(use-package verb
  :defer t)

(use-package websocket-client
  :ensure nil
  :commands websocket-client-open)

(use-package know-your-http-well
  :defer t)


(provide 'init-web)

;;; init-web.el ends here
