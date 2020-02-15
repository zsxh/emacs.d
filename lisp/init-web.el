;; init-web.el --- Web Configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Web Configurations
;;

;;; Code:

;; Json config
(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :hook (json-mode . (lambda ()
                       (make-local-variable 'js-indent-level)
                       (setq js-indent-level 2))))

;; YAML config
(use-package yaml-mode
  :commands yaml-mode
  :config (add-hook 'yaml-mode-hook
                    '(lambda ()
                       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; Web mode for html,xml...
(use-package web-mode
  :commands web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
  :hook ((web-mode . +web/config))
  :config
  (defun +web/config ()
    (unless (and (buffer-file-name)
                 (equal "xml" (file-name-extension (buffer-file-name))))
      (flycheck-mode))
    (setq-local company-backends
                '(company-capf company-files company-css company-dabbrev)))

  ;; Install tidy to check html syntax, https://www.flycheck.org/en/latest/languages.html#html
  (use-package flycheck
    :config
    (flycheck-add-mode 'html-tidy 'web-mode)
    (+funcs/major-mode-leader-keys
     web-mode-map
     "e" '(nil :which-key "error")
     "en" '(flycheck-next-error :which-key "next-error")
     "ep" '(flycheck-previous-error :which-key "prev-error")
     "f" '(format-all-buffer :which-key "format-html")
     "p" '(nil :which-key "preview")
     "pa" '(+web/add-buffer-to-preview :which-key "add-buffer-to-preview")
     "pp" '(+web/preview-in-browser :which-key "preivew-in-browser")))

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
  :defer t
  :hook (css-mode . (lambda ()
                      (setq-local company-backends
                                  '(company-capf company-files company-css company-dabbrev))))
  :config
  (setq css-indent-offset 2))

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

;; https://github.com/manateelazycat/instant-rename-tag
(use-package instant-rename-tag
  :quelpa ((instant-rename-tag :fetcher github :repo "manateelazycat/instant-rename-tag"))
  :commands instant-rename-tag)

;; https://github.com/manateelazycat/highlight-matching-tag
(use-package highlight-matching-tag
  :quelpa ((highlight-matching-tag :fetcher github :repo "manateelazycat/highlight-matching-tag"))
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

(use-package simple-httpd
  :defer t)

;; editing with preview

;; TODO: https://github.com/skeeto/skewer-mode
;; (use-package skewer-mode)

;; https://github.com/skeeto/impatient-mode
;; Enable the web server provided by simple-httpd:
;; M-x httpd-start
;; Publish buffers by enabling the minor mode impatient-mode.
;; M-x impatient-mode
;; And then point your browser to http://localhost:8080/imp/, select a buffer, and watch your changes appear as you type!
(use-package impatient-mode
  :commands (impatient-mode +web/add-buffer-to-preview +web/preview-in-browser)
  :config
  (require 'simple-httpd)
  (defun +web/add-buffer-to-preview ()
    (interactive)
    (impatient-mode 1))
  (defun +web/preview-in-browser ()
    (interactive)
    (when (and (eq major-mode 'web-mode))
      (if (not (httpd-running-p))
          (httpd-start))
      (when (not impatient-mode)
        (+web/add-buffer-to-preview))
      (browse-url "http://localhost:8080/imp/"))))

;; install formatter
;; npm install --global prettier @prettier/plugin-php
(use-package format-all
  :commands (format-all-buffer))

(use-package know-your-http-well
  :defer t)


(provide 'init-web)

;;; init-web.el ends here
