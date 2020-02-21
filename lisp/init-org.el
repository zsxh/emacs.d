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
(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :bind ((:map org-mode-map
               ("C-c C-," . org-insert-structure-template)
               ("C-M-<return>" . org-table-insert-hline)))
  :commands org-open-at-point
  :config
  (setq org-confirm-babel-evaluate nil) ; do not prompt me to confirm everytime I want to evaluate a block
  (setq org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
  (setq org-export-use-babel nil ; do not evaluate again during export.
        org-export-with-toc nil
        org-export-with-section-numbers nil)

  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; org agenda
  (setq org-directory "~/org")
  (setq org-agenda-files '("~/org/gtd"))
  (setq org-default-notes-file (concat org-directory "/gtd/caputure.org"))

  ;; Org table font
  ;; (set-face-attribute 'org-table nil :family "Ubuntu Mono derivative Powerline")
  (when (member "M+ 1m" (font-family-list))
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
                        (org-babel-hide-result-toggle-maybe))))))

;; Org-mode keybindings
(use-package evil-org
  :after (org evil)
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
  (+funcs/major-mode-leader-keys
   org-mode-map
   "a" '(org-agenda :which-key "agenda")
   "b" '(nil :which-key "block")
   "bb" '(play-code-block :which-key "play-code-with-online-playground")
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
   "T" '(nil :which-key "toggle")
   "Ti" '(org-toggle-inline-images :which-key "toggle-inline-images")
   "Tl" '(org-toggle-link-display :which-key "toggle-link-display")
   "Tp" '(+latex/toggle-latex-preview :which-key "toggle-latex-preview")
   "'" '(org-edit-special :which-key "editor")))

;; Org Static Blog
(use-package org-static-blog
  :commands (org-static-blog-mode
             org-static-blog-publish
             org-static-blog-publish-file
             org-static-blog-create-new-post))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Presentation
(use-package org-tree-slide
  :commands org-tree-slide-mode)

;; convert org-file to ipynb
;; https://github.com/jkitchin/ox-ipynb
;; convert ipynb to org-file
;; $pip install nbcorg
(use-package ox-ipynb
  :quelpa ((ox-ipynb :fetcher github :repo "jkitchin/ox-ipynb"))
  :defer t
  :init
  (add-hook 'org-load-hook (lambda () (require 'ox-ipynb))))

;; Org Bable
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/contrib/jupyter.el
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/config.el
(defvar +org/babel-mode-alist
  '((cpp . C)
    (C++ . C)
    (D . C)
    (sh . shell)
    (bash . shell)
    (matlab . octave)
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
  (defun +org/babel-lazy-load (lang)
    (cl-check-type lang symbol)
    (or (run-hook-with-args-until-success '+org/babel-load-functions lang)
        (require (intern (format "ob-%s" lang)) nil t)
        (require lang nil t)))

  (defun +org/src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    (or (cdr (assoc lang org-src-lang-modes))
        (and lang (+org/babel-lazy-load (intern lang)))))

  (advice-add #'org-src-get-lang-mode :before #'+org/src-lazy-load-library-a)

  ;; This also works for tangling and exporting
  (defun +org/babel-lazy-load-library-a (info)
    "Load babel libraries lazily when babel blocks are executed."
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang)
                       ((stringp lang) (intern lang))))
           (lang (or (cdr (assq lang +org/babel-mode-alist))
                     lang)))
      (when (and lang
                 (not (cdr (assq lang org-babel-load-languages)))
                 (+org/babel-lazy-load lang))
        (when (assq :async (nth 2 info))
          ;; ob-async has its own agenda for lazy loading packages (in the
          ;; child process), so we only need to make sure it's loaded.
          (require 'ob-async nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))
      t))

  (advice-add #'org-babel-confirm-evaluate :after-while #'+org/babel-lazy-load-library-a)

  (defun +org/noop-org-babel-do-load-languages-a (&rest _)
    (message
     (concat "`org-babel-do-load-languages' is redundant with lazy loading mechanism for babel "
             "packages. There is no need to use it, so it has been disabled")))

  (advice-add #'org-babel-do-load-languages :override #'+org/noop-org-babel-do-load-languages-a))

(add-hook 'org-load-hook #'+org/init-label-lazy-loader-h)

(use-package ob-go :defer t)
(use-package ob-rust :defer t)
(use-package ob-ipython :defer t)

;; FIXME: don't know how to restart/stop kernel, don't know why emacs not delete subprocess after deleting process
(use-package ob-jupyter
  :ensure jupyter
  :defer t
  :init
  (defun +org/babel-load-jupyter-h (lang)
    (when (string-prefix-p "jupyter-" (symbol-name lang))
      (require 'jupyter)
      (let* ((lang-name (symbol-name lang))
             (lang-tail (string-remove-prefix "jupyter-" lang-name)))
        (and (not (assoc lang-tail org-src-lang-modes))
             (require (intern (format "ob-%s" lang-tail)) nil t)
             (add-to-list 'org-src-lang-modes (cons lang-name (intern lang-tail)))))
      (with-demoted-errors "Jupyter: %s"
        (require lang nil t)
        (require 'ob-jupyter nil t))))
  (add-hook '+org/babel-load-functions #'+org/babel-load-jupyter-h))

(use-package ob-julia
  :defer t
  :quelpa ((ob-julia :fetcher github :repo phrb/ob-julia)))

(use-package ob-restclient
  :defer t
  :ensure restclient)

;; ob-async enables asynchronous execution of org-babel src blocks
(use-package ob-async
  :defer t
  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
            '(lambda ()
               (setq inferior-julia-program-name "julia")))
  ;; emacs jupyter define their own :async keyword that may conflicts with ob-async
  (setq ob-async-no-async-languages-alist
        '("jupyter-python" "jupyter-julia" "jupyter-javascript")))


(provide 'init-org)

;;; init-org.el ends here
