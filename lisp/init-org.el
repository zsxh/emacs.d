;; init-org.el --- Org Configuations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Org
;;

;;; Code:

;; The Org Manual: https://orgmode.org/org.html
;; https://orgmode.org/Changes.html
;; https://org-babel.readthedocs.io/en/latest/header-args/
(use-package org
  :ensure nil
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :bind ((:map org-mode-map
               ("C-c C-," . org-insert-structure-template)
               ("C-M-<return>" . org-table-insert-hline)
               ("C-j" . org-return)))
  ;; :commands org-open-at-point
  :preface
  ;; customize before (require 'org)
  (setq org-emphasis-regexp-components '("-[:space:][:nonascii:]('\"{"
                                         "-[:space:][:nonascii:].,:!?;'\")}\\["
                                         "[:space:]"
                                         "."
                                         1))
  :config
  (setq org-confirm-babel-evaluate nil) ; do not prompt me to confirm everytime I want to evaluate a block
  ;; (setq org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
  (setq org-export-use-babel nil ; do not evaluate again during export.
        org-export-with-toc nil
        org-export-with-section-numbers nil
        org-hide-emphasis-markers t
        ;; NOTE: do not load used org modules to speed up first time load
        ;; https://orgmode.org/manual/Structure-Templates.html
        org-modules '(ol-bibtex org-tempo))
  ;; org restore window configuration after org-edit-src-exit
  ;; https://www.reddit.com/r/orgmode/comments/f9qy5h/in_orgmode_when_editing_a_source_block_with/
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2019-12/msg00263.html
  (setq org-src-window-setup 'other-window)
  (setq org-startup-with-inline-images t
        org-image-actual-width nil)

  (setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                               (vm-imap . vm-visit-imap-folder-other-frame)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file)
                               (wl . wl-other-frame)))

  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; org agenda
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/gtd"))
  (setq org-default-notes-file (concat org-directory "/gtd/caputure.org"))

  ;; org todo keyword
  (setq org-todo-keywords
        '((type "TODO" "DOING" "MAYBE" "|" "DONE" "DROP")))

  (setq org-todo-keyword-faces
        '(("DOING" . (:foreground "#dc322f" :weight bold :underline t))
          ("MAYBE" . (:foreground "#ECBE7B" :weight bold))
          ("DROP" . (:foreground "#96A7A9" :weight bold :strike-through t))))

  ;; Org table font
  ;; (set-face-attribute 'org-table nil :family "Ubuntu Mono derivative Powerline")
  (when (and (not (display-graphic-p))
             (member "M+ 1m" (font-family-list)))
    ;; Download font https://mplus-fonts.osdn.jp/about-en.html
    (set-face-attribute 'org-table nil :family "M+ 1m"))

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
    (org-babel-show-result-all))

  (defun +org/babel-result-hide-all ()
    "Fold all results in the current buffer."
    (interactive)
    (org-babel-show-result-all)
    (save-excursion
      ;; org-babel-result-hide-all may not work without (goto-char (point-min))
      (goto-char (point-min))
      (while (re-search-forward org-babel-result-regexp nil t)
        (save-excursion (goto-char (match-beginning 0))
                        (org-babel-hide-result-toggle-maybe)))))

  ;; zero-width spaces
  (define-key org-mode-map (kbd "M-SPC M-SPC")
              (lambda () (interactive) (insert "\u200b")))

  (with-eval-after-load 'ox
    (defun +org/export-remove-zero-width-space (text _backend _info)
      "Remove zero width spaces from TEXT."
      (unless (org-export-derived-backend-p 'org)
        (replace-regexp-in-string "\u200b" "" text)))

    (add-to-list 'org-export-filter-final-output-functions #'+org/export-remove-zero-width-space t)))

;; https://github.com/xenodium/org-block-capf
(use-package org-block-capf
  :ensure nil
  :init (slot/vc-install :fetcher "github" :repo "xenodium/org-block-capf")
  :custom
  (org-block-capf-edit-style 'inline)
  :hook (org-mode . org-block-capf-add-to-completion-at-point-functions))

(use-package org-habit
  :ensure nil
  :after org-agenda
  :config
  (setq org-habit-show-habits-only-for-today t
        org-habit-show-all-today t))

;; Toggle visibility of hidden Org mode element parts upon entering and leaving an element
;; https://github.com/awth13/org-appear
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-delay 0.5))

;; Org-mode keybindings
(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme)

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

  ;; Open links and files with RET in normal state
  (evil-define-key 'normal org-mode-map
    (kbd "RET") 'org-open-at-point)

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
  (+funcs/major-mode-leader-keys
   org-mode-map
   ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Echo.html
   "." '(display-local-help :which-key "display-local-help")
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
   "e" '(org-export-dispatch :which-key "export")
   "i" '(nil :which-key "insert")
   "is" '(org-insert-structure-template :which-key "structure-template")
   "it" '(org-time-stamp :which-key "time-stamp")
   "n" '(org-babel-next-src-block :which-key "next-src-block")
   "p" '(org-babel-previous-src-block :which-key "previous-src-block")
   "r" '(transient-org-roam :which-key "org-roam-menu")
   "T" '(nil :which-key "toggle")
   "Ti" '(org-toggle-inline-images :which-key "toggle-inline-images")
   "Tl" '(org-toggle-link-display :which-key "toggle-link-display")
   "Tp" '(+latex/toggle-latex-preview :which-key "toggle-latex-preview")
   "'" '(org-edit-special :which-key "editor")))

;; https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '(?◉ ?🞛 ?○ ?▷)))

;; Presentation
(use-package org-tree-slide
  :commands org-tree-slide-mode)

;; Org Bable
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/contrib/jupyter.el
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/config.el
(defvar +org/babel-mode-alist
  '((cpp . C)
    (C++ . C)
    (D . C)
    (elisp . emacs-lisp)
    (sh . shell)
    (bash . shell)
    (matlab . octave)
    ;; I'm not using rustic-mode now
    ;; (rust . rustic-babel)
    (amm . ammonite))
  "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.
For example, with (fish . shell) will cause #+BEGIN_SRC fish to load ob-shell.el
when executed.")

(defvar +org/babel-load-functions ()
  "A list of functions executed to load the current executing src block. They
take one argument (the language specified in the src block, as a string). Stops
at the first function to return non-nil.")

(defun +org/init-label-lazy-loader-h ()
  "Load label libraries lazily when babel blocks are executed."

  ;; It doesn't matter what +org/babel-lazy-load return
  (defun +org/babel-lazy-load (lang &optional async)
    (cl-check-type lang symbol)
    (when (and async (not (featurep 'ob-async))
               ;; ob-async has its own agenda for lazy loading packages (in the
               ;; child process), so we only need to make sure it's loaded.
               (require 'ob-async nil t)))
    (unless (cdr (assq lang org-babel-load-languages))
      (prog1
          (or (run-hook-with-args-until-success '+org/babel-load-functions lang)
              (require (intern (format "ob-%s" lang)) nil t)
              (require lang nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))))

  (defun +org/export-lazy-load-library-h ()
    (+org/babel-lazy-load-library-a (org-babel-get-src-block-info)))

  (advice-add 'org-babel-exp-src-block :before '+org/export-lazy-load-library-h)

  (defun +org/src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    (when lang
      (or (cdr (assoc lang org-src-lang-modes))
          (+org/babel-lazy-load (cond ((symbolp lang) lang)
                                      ((stringp lang) (intern lang)))))))

  (advice-add #'org-src-get-lang-mode :before #'+org/src-lazy-load-library-a)

  ;; This also works for tangling and exporting
  (defun +org/babel-lazy-load-library-a (info)
    "Load babel libraries lazily when babel blocks are executed."
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang)
                       ((stringp lang) (intern lang))))
           (lang (or (cdr (assq lang +org/babel-mode-alist))
                     lang)))
      (+org/babel-lazy-load lang (assq :async (nth 2 info)))
      t))

  (advice-add #'org-babel-confirm-evaluate :after-while #'+org/babel-lazy-load-library-a)

  (advice-add #'org-babel-do-load-languages :override #'ignore))

(add-hook 'org-load-hook #'+org/init-label-lazy-loader-h)

(use-package ob-go :defer t)
(use-package ob-rust :defer t)
(use-package ob-restclient :defer t)

;; https://github.com/mermaid-js/mermaid-cli
;; https://github.com/arnm/ob-mermaid
;;
;; $ mkdir -p ~/.emacs.d/cache/ob-mermaid & cd ~/.emacs.d/cache/ob-mermaid & npm install -g @mermaid-js/mermaid-cli
;;
;; #+begin_src mermaid :file ob-mermaid/test.svg
;; sequenceDiagram
;;   A-->B: Works!
;; #+end_src
(use-package ob-mermaid
  :defer t
  :config
  (setq ob-mermaid-cli-path (expand-file-name "cache/ob-mermaid/node_modules/.bin/mmdc" user-emacs-directory)))

;; http://plantuml.sourceforge.net/
;; $ mkdir -p ~/.emacs.d/cache/ob-plantuml & wget -c http://sourceforge.net/projects/plantuml/files/plantuml.jar/download -O ~/.emacs.d/cache/ob-plantuml/plantuml.jar
;; $ sudo pacman -S graphviz
(use-package ob-plantuml
  :ensure nil
  :defer t
  :custom (org-plantuml-jar-path (expand-file-name "cache/ob-plantuml/plantuml.jar" user-emacs-directory)))

;; TODO: use org-mode built-in async,check `https://blog.tecosaur.com/tmio/2021-05-31-async.html'
;; ob-async enables asynchronous execution of org-babel src blocks
(use-package ob-async
  :defer t
  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
            (lambda ()
              (setq inferior-julia-program-name "julia")))
  ;; emacs jupyter define their own :async keyword that may conflicts with ob-async
  (setq ob-async-no-async-languages-alist
        '("ipython"
          "jupyter-python"
          "jupyter-julia"
          "jupyter-R"
          "jupyter-javascript")))

;; Ory Static Blog
(use-package org-static-blog
  :commands (org-static-blog-mode
             org-static-blog-publish
             org-static-blog-publish-file
             org-static-blog-create-new-post)
  :config
  ;; https://github.com/bastibe/.emacs.d/blob/master/init.el#L729
  (setq org-static-blog-publish-url "https://zsxh.github.io/"
        org-static-blog-publish-title "zsxh blog"
        org-static-blog-publish-directory "~/org/blog/"
        org-static-blog-posts-directory "~/org/blog/posts/"
        org-static-blog-drafts-directory "~/org/blog/drafts/"
        org-static-blog-use-preview t
        org-static-blog-preview-ellipsis "Read more..."
        org-static-blog-preview-link-p t
        org-static-blog-enable-tags t)
  (load (expand-file-name "site-lisp/org-static-blog-custom.el" user-emacs-directory)))


(provide 'init-org)

;;; init-org.el ends here
