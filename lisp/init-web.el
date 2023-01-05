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
  (define-derived-mode pom-xml-mode web-mode "POM-XML"
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
         ("\\.xml?\\'" . web-mode)
         ("pom.xml?\\'" . pom-xml-mode))
  :hook ((web-mode . +web/config))
  :config
  (defun +web/config ()
    (setq-local company-backends
                '(company-capf company-files company-css company-dabbrev)))

  (defun +web/formatter ()
    (interactive)
    (format-all-buffer))

  (+funcs/major-mode-leader-keys web-mode-map
                                 "e" '(nil :which-key "error")
                                 "f" '(+web/formatter :which-key "format-html")
                                 "p" '(nil :which-key "preview"))

  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-sql-indent-offset 2)

  (+funcs/major-mode-leader-keys
   web-mode-map
   "r" 'instant-rename-tag))

(use-package css-mode
  :ensure nil
  :hook ((css-mode css-ts-mode) . (lambda ()
                                    (setq-local company-backends
                                                '(company-capf company-files company-css company-dabbrev))))
  :config
  (setq css-indent-offset 2)
  (dolist (mode-map '(css-mode-map css-ts-mode-map))
    (+funcs/major-mode-leader-keys
     mode-map
     "c" '(css-cycle-color-format :which-key "css-cycle-color-format"))))

;; edit html
(use-package sgml-mode
  :ensure nil
  :commands sgml-slash
  :config
  ;; https://emacs.stackexchange.com/questions/33240/html-mode-that-closes-tags
  ;; automatic insertion of the closing tag if you type </ or
  ;; pressing C-c / or C-c C-e or C-c / inserts a closing tag (the whole </foo>).
  (setq sgml-quick-keys 'close)

  (defun +web/sgml-slash (arg)
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
      ;; also delete the last ?> char if `electric-pair-mode' is enabled
      (when (and electric-pair-mode
                 (eq ?> (char-after)))
        (delete-char 1))
      (if (eq ?> (char-before))
          (save-excursion
            (sgml-close-tag))
        (sgml-close-tag)))
     (t
      (insert-char ?/ arg))))

  (advice-add 'sgml-slash :override '+web/sgml-slash))

;; https://github.com/manateelazycat/highlight-matching-tag
(use-package highlight-matching-tag
  :ensure nil
  :init (slot/vc-install :fetcher "github" :repo "manateelazycat/highlight-matching-tag")
  :commands highlight-matching-tag
  :hook (web-mode . (lambda () (highlight-matching-tag 1))))

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
