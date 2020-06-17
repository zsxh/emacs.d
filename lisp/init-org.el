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
  :preface
  ;; customize before (require 'org)
  (setq org-emphasis-regexp-components '("-[:space:][:nonascii:]('\"{"
                                         "-[:space:][:nonascii:].,:!?;'\")}\\["
                                         "[:space:]"
                                         "."
                                         1))
  :config
  (setq org-confirm-babel-evaluate nil) ; do not prompt me to confirm everytime I want to evaluate a block
  (setq org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
  (setq org-export-use-babel nil ; do not evaluate again during export.
        org-export-with-toc nil
        org-export-with-section-numbers nil
        org-hide-emphasis-markers nil)
  ;; org restore window configuration after org-edit-src-exit
  ;; https://www.reddit.com/r/orgmode/comments/f9qy5h/in_orgmode_when_editing_a_source_block_with/
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2019-12/msg00263.html
  (setq org-src-window-setup 'other-window)
  (setq org-startup-with-inline-images t)

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

  (with-eval-after-load 'eaf
    (defun +org/eaf-open-file (file-path link-without-schema)
      (eaf-open file-path))

    (setq org-file-apps '((auto-mode . emacs)
                          ("\\.mm\\'" . default)
                          ("\\.x?html?\\'" . default)
                          ("\\.gif\\'" . +org/eaf-open-file)
                          ("\\.png\\'" . +org/eaf-open-file)
                          ("\\.jpe?g\\'" . +org/eaf-open-file)
                          ("\\.pdf\\'" . +org/eaf-open-file)))))

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
   "bb" '(playonline :which-key "play-code-with-online-playground")
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

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Presentation
(use-package org-tree-slide
  :commands org-tree-slide-mode)

;; This package provides visual alignment for Org tables on GUI Emacs.
;; https://github.com/casouri/valign
(use-package valign
  :quelpa ((valign :fetcher github :repo "casouri/valign"))
  :after org
  :config
  (valign-mode)
  (advice-add 'text-scale-increase
              :after (lambda (inc)
                       (when (or (bound-and-true-p valign-mode)
                                 (derived-mode-p 'org-mode)
                                 (derived-mode-p 'markdown-mode))
                         (valign--force-align-buffer))))
  (advice-add 'text-scale-decrease
              :after (lambda (dec)
                       (when (or (bound-and-true-p valign-mode)
                                 (derived-mode-p 'org-mode)
                                 (derived-mode-p 'markdown-mode))
                         (valign--force-align-buffer)))))

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
    (unless (cdr (assq lang org-babel-load-languages))
      (prog1
       (or (run-hook-with-args-until-success '+org/babel-load-functions lang)
           (require (intern (format "ob-%s" lang)) nil t)
           (require lang nil t))
       (add-to-list 'org-babel-load-languages (cons lang t)))))

  (defun +org/src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    (or (cdr (assoc lang org-src-lang-modes))
        (+org/babel-lazy-load (intern lang))))

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
(use-package ob-restclient :defer t)

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

;; Ory Static Blog
(use-package org-static-blog
  :commands (org-static-blog-mode
             org-static-blog-publish
             org-static-blog-publish-file
             org-static-blog-create-new-post))

(with-eval-after-load 'org-static-blog
  ;; https://github.com/bastibe/.emacs.d/blob/master/init.el#L729
  (setq org-static-blog-publish-url "https://zsxh.github.io/"
        org-static-blog-publish-title "Zsxh Blog"
        org-static-blog-publish-directory "~/org/blog/"
        org-static-blog-posts-directory "~/org/blog/posts/"
        org-static-blog-drafts-directory "~/org/blog/drafts/"
        org-static-blog-use-preview nil
        org-static-blog-enable-tags t)

  (defun org-static-blog-generate-post-path (post-filename post-datetime)
    (concat "html/" (file-name-nondirectory post-filename)))

  (setcdr (assoc "en"
                 (assoc 'date-format
                        org-static-blog-texts))
          "%Y-%m-%d")

  (defun org-static-blog-get-post-summary (post-filename)
    "Assemble post summary for an archive page.
This function is called for every post on the archive and
tags-archive page. Modify this function if you want to change an
archive headline."
    (concat
     "<h2 class=\"post-title\">"
     (format-time-string (org-static-blog-gettext 'date-format) (org-static-blog-get-date post-filename))
     "&ensp;"
     "<a href=\"" (org-static-blog-get-post-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>"
     "</h2>"))

  (defun org-static-blog-post-preamble (post-filename)
    "Returns the formatted date and headline of the post.
This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline."
    (let ((created-date (format-time-string (org-static-blog-gettext 'date-format)
                                            (org-static-blog-get-date post-filename)))
          (updated-date (format-time-string (org-static-blog-gettext 'date-format)
                                            (nth 5 (file-attributes post-filename)))))
      (concat
       "<h1 class=\"post-title\">"
       "<a href=\"" (org-static-blog-get-post-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>"
       "</h1>\n"
       "<div class=\"post-date\"> Posted: " created-date
       (if (string-equal created-date updated-date)
           ""
         (format "&ensp;Updated: %s" updated-date))
       "</div>")))

  (setq org-static-blog-page-header
        "<meta name=\"author\" content=\"ZSXH\">
<meta name=\"referrer\" content=\"no-referrer\">
<link href= \"/static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
<script src=\"/static/katex.min.js\"></script>
<script src=\"/static/auto-render.min.js\"></script>
<link rel=\"stylesheet\" href=\"/static/katex.min.css\">
<script>document.addEventListener(\"DOMContentLoaded\", function() { renderMathInElement(document.body); });</script>
<meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\">
<meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">")

  (setq org-static-blog-page-preamble
        "<div class=\"header\">
<a href=\"/index.html\">Home</a> |
<a href=\"/archive.html\">Archive</a> |
<a href=\"/html/resource.html\">Resources</a> |
<a href=\"https://github.com/zsxh\">Github</a> |
<a href=\"/about.html\">About</a> |
<a href=\"/rss.xml\">RSS</a>
</div>")

  (setq org-static-blog-page-postamble
        "
<center><button id=\"disqus_button\" onclick=\"load_disqus()\">Load Disqus Comments</button></center>
<div id=\"disqus_thread\"></div>
<script type=\"text/javascript\">
    function load_disqus() {
        var dsq = document.createElement('script');
        dsq.type = 'text/javascript';
        dsq.async = true;
        dsq.src = 'https://zsxhbnbvb.disqus.com/embed.js';
        (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
        document.getElementById('disqus_button').style.visibility = 'hidden';
    };
</script>
<center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">zsxh.github.io</span> by <a xmlns:cc=\"https://creativecommons.org/ns#\" href=\"https://zsxh.github.io\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">ZSXH</a> is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>"))


(provide 'init-org)

;;; init-org.el ends here
