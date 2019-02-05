;; init-org.el --- Org Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Org
;;

;;; Code:

;; https://orgmode.org/Changes.html
(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("C-c C-," . org-insert-structure-template))
  :commands org-open-at-point
  :config
  (setq org-confirm-babel-evaluate nil) ;don't prompt me to confirm everytime I want to evaluate a block

  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; org agenda
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/gtd"))
  (setq org-default-notes-file (concat org-directory "/gtd/caputure.org"))

  ;; Org table font
  (custom-set-faces
   '(org-table ((t (:family "Ubuntu Mono derivative Powerline")))))

  ;; Org block face
  (set-face-background 'org-block "#E0E0E0")
  (set-face-background 'org-quote nil)
  (set-face-background 'org-block-begin-line nil)
  (set-face-background 'org-block-end-line nil)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; enable ob-*lang* yourself

  ;; ob-sh renamed to ob-shell since 26.1.
  (when (>= emacs-major-version 26)
    (require 'ob-shell))

  (if (>= emacs-major-version 26)
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (use-package ob-go
    :ensure t
    :after org
    :init (cl-pushnew '(go . t) load-language-list))

  (use-package ob-rust
    :ensure t
    :after org
    :init (cl-pushnew '(rust . t) load-language-list))

  ;; https://github.com/gregsexton/ob-ipython
  ;; enable ob-ipython and ob-python
  (use-package ob-ipython
    :ensure t
    :after org
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list)
    :config
    ;; fix: pandas dataframe output header is not aligned to column
    ;; https://github.com/gregsexton/ob-ipython/issues/171
    (defun ob-ipython--render (file-or-nil values)
      (let ((org (lambda (value) value))
            (png (lambda (value)
                   (let ((file (or file-or-nil (ob-ipython--generate-file-name ".png"))))
                     (ob-ipython--write-base64-string file value)
                     (format "[[file:%s]]" file))))
            (svg (lambda (value)
                   (let ((file (or file-or-nil (ob-ipython--generate-file-name ".svg"))))
                     (ob-ipython--write-string-to-file file value)
                     (format "[[file:%s]]" file))))
            (html (lambda (value)))
            (txt (lambda (value)
                   (let ((lines (s-lines value)))
                     (if (cdr lines)
                         (->> lines
                              (-map 's-trim-right)
                              (s-join "\n  ")
                              (s-concat "  ")
                              (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE"))
                       (s-concat ": " (car lines)))))))
        (or (-when-let (val (cdr (assoc 'text/org values))) (funcall org val))
            (-when-let (val (cdr (assoc 'image/png values))) (funcall png val))
            (-when-let (val (cdr (assoc 'image/svg+xml values))) (funcall svg val))
            (-when-let (val (cdr (assoc 'text/plain values))) (funcall txt val))))))

  ;; An extension to restclient.el for emacs that provides org-babel support
  (when (package-installed-p 'restclient)
    (use-package ob-restclient
      :ensure t
      :after org
      :init (cl-pushnew '(restclient . t) load-language-list)))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; lsp support org code block editing
  (defvar org-babel-lang-list
    '("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java"))

  (add-to-list 'org-babel-lang-list (if (>= emacs-major-version 26) "shell" "sh"))

  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enbale ,lang))))

;; Org-mode keybindings
(use-package evil-org
  :after (org evil)
  :ensure t
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)
              ;; Open links and files with RET in normal state
              (evil-define-key 'normal org-mode-map
                (kbd "RET") 'org-open-at-point)))

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-define-key 'motion org-agenda-mode-map
    "?" 'org-agenda-view-mode-dispatch
    "0" 'digit-argument)

  (with-eval-after-load 'org-src
    (evil-define-minor-mode-key 'normal 'org-src-mode
      ",c" 'org-edit-src-exit
      ",k" 'org-edit-src-abort))

  (with-eval-after-load 'org-capture
    (evil-define-minor-mode-key 'normal 'org-capture-mode
      ",c" 'org-capture-finalize
      ",k" 'org-capture-kill
      ",w" 'org-capture-refile))

  ;; major mode keybindings
  (+funcs/set-leader-keys-for-major-mode
   'org-mode-map
   "a" '(org-agenda :which-key "agenda")
   "b" '(nil :which-key "block")
   "bf" '(+org/babel-result-hide-all :which-key "fold-all-results")
   "bF" '(+org/babel-result-show-all :which-key "show-all-results")
   "br" '(org-babel-remove-result :which-key "remove-result")
   "bR" '(+org/remove-all-result-blocks :which-key "remove-all-results")
   "c" '(nil :which-key "capture/clock")
   "cc" '(org-capture :which-key "capture")
   "ci" '(org-clock-in :which-key "clock-in")
   "co" '(org-clock-out :which-key "clock-out")
   "cr" '(org-clock-report :which-key "clock-report")
   "i" '(nil :which-key "insert")
   "is" '(org-insert-structure-template :which-key "structure-template")
   "it" '(org-time-stamp :which-key "time-stamp")
   "n" '(org-babel-next-src-block :which-key "next-src-block")
   "p" '(org-babel-previous-src-block :which-key "previous-src-block")
   "T" '(nil :which-key "toggle")
   "Ti" '(org-toggle-inline-images :which-key "toggle-inline-images")
   "Tl" '(org-toggle-link-display :which-key "toggle-link-display")
   "'" '(org-edit-special :which-key "editor")))

;; Org for blog
(use-package org-page
  :after org
  :config
  (setq op/repository-directory "~/org/zsxh.github.io")
  (setq op/site-domain "https://zsxh.github.io/")
  (setq op/personal-disqus-shortname "zsxhspace")
  (setq op/site-main-title "Hello World的一千种写法")

  (setq op/repository-org-branch "source")  ;; default is "source"
  (setq op/repository-html-branch "master") ;; default is "master"

  (setq op/personal-github-link "https://github.com/zsxh")

  (setq op/personal-google-analytics-id "UA-119871562-1")

  ;; (setq op/highlight-render 'htmlize)
  (setq op/theme 'mdo))

;; ob-async enables asynchronous execution of org-babel src blocks
(use-package ob-async
  :after org
  :ensure t
  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
            '(lambda ()
               (setq inferior-julia-program-name "julia")))
  ;; ob-python define their own :async keyword that conflicts with ob-async
  (setq ob-async-no-async-languages-alist '("ipython")))

(with-eval-after-load 'org
  (defun +org/remove-all-result-blocks ()
    "Remove all results in the current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "#+begin_src " nil t)
        (org-babel-remove-result))))

  (defun +org/babel-result-show-all ()
    "Show all results in the current buffer."
    (interactive)
    (org-babel-show-result-all)
    (when (featurep 'ob-ipython)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward ":results:" nil t)
          (forward-line)
          (org-reveal)))))

  (defun +org/babel-result-hide-all ()
    "Fold all results in the current buffer."
    (interactive)
    (org-babel-show-result-all)
    (save-excursion
      ;; org-babel-result-hide-all may not work without (goto-char (point-min))
      (goto-char (point-min))
      (while (re-search-forward org-babel-result-regexp nil t)
        (save-excursion (goto-char (match-beginning 0))
                        (org-babel-hide-result-toggle-maybe))))))


(provide 'init-org)

;;; init-org.el ends here
