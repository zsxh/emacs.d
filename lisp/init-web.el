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
  :ensure t
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
  :ensure t
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
  ;; :hook (web-mode . lsp)
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-attr-indent-offset 2
          web-mode-sql-indent-offset 2))
  (add-hook 'web-mode-hook 'my-web-mode-hook))

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
  :ensure t
  :commands restclient-mode
  :config
  (use-package company-restclient
    :ensure t
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
#
# POST https://jira.atlassian.com/rest/api/2/search
# Content-Type: application/json
# {}\n")))
    (switch-to-buffer restclient-buffer)))


(provide 'init-web)

;;; init-web.el ends here
