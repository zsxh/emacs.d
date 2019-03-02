;; init-completion.el --- commpletion configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Code Completion Configuations
;;

;;; Code:

(use-package company
  :ensure t
  ;; :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind (("M-/" . yas-expand)
         ("C-c C-y" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("C-k" . company-select-previous)
         ("C-j" . company-select-next)
         ("TAB" . company-complete-common)
         ("<tab>" . company-complete-common)
         ("<backtab>" . company-select-previous)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay 0.01 ; decrease delay before autocompletion popup shows
        ;; company-echo-delay 0  ; remove annoying blinking
        company-minimum-prefix-length 1
        ;; company-require-match nil
        ;; company-dabbrev-ignore-case nil
        ;; company-dabbrev-downcase nil
        ))

;; Fuzzy complete
(use-package flx
  :ensure t
  :defer t)

(use-package company-flx
  :after company
  :ensure t
  :config (company-flx-mode 1))

(use-package yasnippet
  :ensure t
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (with-eval-after-load 'snippet
    (+funcs/set-leader-keys-for-major-mode
     snippet-mode-map
     "t" '(yas-tryout-snippet :which-key "yas-tryout-snippet"))))

;; replace `company-quickhelp' with `company-box-doc'
;; otherwise `company-box-doc' will have performance issue https://github.com/sebastiencs/company-box/issues/19
(use-package company-box
  :if (and (>= emacs-major-version 26) (display-graphic-p))
  :ensure t
  :diminish
  :functions (all-the-icons-faicon
              all-the-icons-material
              all-the-icons-octicon
              all-the-icons-alltheicon)
  :hook (company-mode . company-box-mode)
  :init (setq company-box-enable-icon (display-graphic-p))
  :config
  (with-eval-after-load 'all-the-icons
    (setq company-box-backends-colors nil
          company-box-max-candidates 50
          company-box-icons-unknown (all-the-icons-octicon "file-text" :v-adjust -0.05 :face 'all-the-icons-purple)
          company-box-icons-yasnippet (all-the-icons-faicon "code" :v-adjust -0.0575 :face 'all-the-icons-green)
          company-box-icons-elisp
          (list
           (all-the-icons-faicon "cube" :v-adjust -0.0575 :face 'all-the-icons-purple) ; Function
           (all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'all-the-icons-blue) ; Variable
           (all-the-icons-faicon "cog" :v-adjust -0.0575 :face 'all-the-icons-orange) ; Feature
           (all-the-icons-material "palette" :v-adjust -0.2 :face 'all-the-icons-pink) ; Face
           )
          company-box-icons-lsp
          `((1 . ,(all-the-icons-faicon "file-text-o" :v-adjust -0.0575 :face 'all-the-icons-green)) ; text
            (2 . ,(all-the-icons-faicon "cube" :v-adjust -0.0575 :face 'all-the-icons-purple)) ; method
            (3 . ,(all-the-icons-faicon "cube" :v-adjust -0.0575 :face 'all-the-icons-purple)) ; function
            (4 . ,(all-the-icons-faicon "cube" :v-adjust -0.0575 :face 'all-the-icons-purple)) ; constructor
            (5 . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'all-the-icons-blue)) ; field
            (6 . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'all-the-icons-blue)) ; variable
            (7 . ,(all-the-icons-faicon "cog" :v-adjust -0.0575 :face 'all-the-icons-orange)) ; class
            (8 . ,(all-the-icons-faicon "cogs" :v-adjust -0.0575 :face 'all-the-icons-orange)) ; interface
            (9 . ,(all-the-icons-alltheicon "less" :face 'all-the-icons-orange)) ; module
            (10 . ,(all-the-icons-faicon "wrench" :v-adjust -0.0575 :face 'all-the-icons-dred)) ; property
            (11 . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'all-the-icons-blue)) ; unit
            (12 . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'all-the-icons-blue)) ; value
            (13 . ,(all-the-icons-material "content_copy" :v-adjust -0.2 :face 'all-the-icons-blue-alt)) ; enum
            (14 . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'all-the-icons-blue)) ; keyword
            (15 . ,(all-the-icons-material "content_paste" :v-adjust -0.2 :face 'all-the-icons-blue-alt)) ; snippet
            (16 . ,(all-the-icons-material "palette" :v-adjust -0.2 :face 'all-the-icons-pink)) ; color
            (17 . ,(all-the-icons-faicon "file" :v-adjust -0.0575 :face 'all-the-icons-lcyan)) ; file
            (18 . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'all-the-icons-blue)) ; reference
            (19 . ,(all-the-icons-faicon "folder" :v-adjust -0.0575 :face 'all-the-icons-lcyan)) ; folder
            (20 . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'all-the-icons-blue)) ; enumMember
            (21 . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'all-the-icons-blue)) ; constant
            (22 . ,(all-the-icons-faicon "cog" :v-adjust -0.0575 :face 'all-the-icons-orange)) ; struct
            (23 . ,(all-the-icons-faicon "bolt" :v-adjust -0.0575 :face 'all-the-icons-yellow)) ; event
            (24 . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'all-the-icons-blue)) ; operator
            (25 . ,(all-the-icons-faicon "tag" :v-adjust -0.0575 :face 'all-the-icons-blue)) ; TypeParameter
            ))))

;; Popup documentation for completion candidates
(use-package company-quickhelp
  :if (and (< emacs-major-version 26) (display-graphic-p))
  :after company
  :ensure t
  :bind (:map company-active-map
              ("M-h" . company-quickhelp-manual-begin))
  :hook (global-company-mode . company-quickhelp-mode)
  :config (setq company-quickhelp-delay 0.3))


(provide 'init-completion)

;;; init-completion.el ends here
