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
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-k" . company-select-previous)
         ("C-j" . company-select-next)
         ("TAB" . company-complete-common)
         ("<tab>" . company-complete-common)
         ("<backtab>" . company-select-previous)
         :map company-search-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("C-k" . company-select-previous)
         ("C-j" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay 0.01 ; decrease delay before autocompletion popup shows
        company-echo-delay 0  ; remove annoying blinking
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
    (defun +company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'ElispFunction)
                ((boundp sym) 'ElispVariable)
                ((featurep sym) 'ElispFeature)
                ((facep sym) 'ElispFace)))))

    (defun my-company-box-icon (family icon face &rest args)
      "Defines icons using `all-the-icons' for `company-box'."
      (when icon
        (pcase family
          ('octicon (all-the-icons-octicon icon :height 0.8 :v-adjust -0.05 :face face args))
          ('faicon (all-the-icons-faicon icon :height 0.8 :v-adjust -0.0575 :face face))
          ('material (all-the-icons-material icon :height 0.8 :v-adjust -0.225 :face face args))
          ('alltheicon (all-the-icons-alltheicon icon :height 0.8 :face face args)))))

    (setq company-box-icons-alist 'company-box-icons-all-the-icons
          company-box-backends-colors nil
          company-box-max-candidates 50
          company-box-icons-functions
          '(company-box-icons--yasnippet company-box-icons--lsp +company-box-icons--elisp company-box-icons--acphp)
          company-box-icons-all-the-icons
          `((Unknown . ,(my-company-box-icon 'material "find_in_page" 'all-the-icons-purple))
            (Text . ,(my-company-box-icon 'material "text_fields" 'all-the-icons-green))
            (Method . ,(my-company-box-icon 'material "functions" 'all-the-icons-red-alt))
            (Function . ,(my-company-box-icon 'material "functions" 'all-the-icons-red-alt))
            (Constructor . ,(my-company-box-icon 'material "functions" 'all-the-icons-red-alt))
            (Field . ,(my-company-box-icon 'material "check_circle" 'all-the-icons-blue))
            (Variable . ,(my-company-box-icon 'material "check_circle" 'all-the-icons-blue))
            (Class . ,(my-company-box-icon 'faicon "cog" 'all-the-icons-orange))
            (Interface . ,(my-company-box-icon 'faicon "info" 'all-the-icons-orange))
            (Module . ,(my-company-box-icon 'faicon "cogs" 'all-the-icons-orange))
            (Property . ,(my-company-box-icon 'material "settings" 'all-the-icons-dyellow))
            (Unit . ,(my-company-box-icon 'faicon "tag" 'all-the-icons-orange))
            (Value . ,(my-company-box-icon 'material "filter_none" 'all-the-icons-blue))
            (Enum . ,(my-company-box-icon 'faicon "list-ul" 'all-the-icons-lcyan))
            (Keyword . ,(my-company-box-icon 'material "filter_center_focus" 'all-the-icons-red))
            (Snippet . ,(my-company-box-icon 'faicon "code" 'all-the-icons-green))
            (Color . ,(my-company-box-icon 'material "color_lens" 'all-the-icons-pink))
            (File . ,(my-company-box-icon 'material "insert_drive_file" 'all-the-icons-dsilver))
            (Reference . ,(my-company-box-icon 'material "collections_bookmark" 'all-the-icons-red))
            (Folder . ,(my-company-box-icon 'material "folder_open" 'all-the-icons-dsilver))
            (EnumMember . ,(my-company-box-icon 'material "people" 'all-the-icons-lcyan))
            (Constant . ,(my-company-box-icon 'material "pause_circle_filled" 'all-the-icons-blue))
            (Struct . ,(my-company-box-icon 'material "streetview" 'all-the-icons-red))
            (Event . ,(my-company-box-icon 'material "event" 'all-the-icons-red))
            (Operator . ,(my-company-box-icon 'material "control_point" 'all-the-icons-red))
            (TypeParameter . ,(my-company-box-icon 'material "class" 'all-the-icons-red))
            (Template   . ,(my-company-box-icon 'faicon "code" 'all-the-icons-green))
            (Yasnippet . ,(my-company-box-icon 'faicon "code" 'all-the-icons-green))
            (ElispFunction . ,(my-company-box-icon 'material "functions" 'all-the-icons-red))
            (ElispVariable . ,(my-company-box-icon 'material "check_circle" 'all-the-icons-blue))
            (ElispFeature . ,(my-company-box-icon 'material "stars" 'all-the-icons-orange))
            (ElispFace . ,(my-company-box-icon 'material "format_paint" 'all-the-icons-pink))))))

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
