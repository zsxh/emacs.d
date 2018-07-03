;; -*- lexical-binding:t -*-

;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;; Font
(set-frame-font "SF Mono 13" nil t)

;; org table font
(custom-set-faces
 '(org-table ((t (:family "Ubuntu Mono derivative Powerline"))))
 )

;; Line Number
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

;; Emacs startup *scratch* buffer
(setq initial-buffer-choice t)

;; Numbered window shortcuts
(use-package window-numbering
  :ensure t
  :hook (after-init . window-numbering-mode))


(provide 'init-ui)
