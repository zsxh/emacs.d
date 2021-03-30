;; init-completion.el --- commpletion configurations	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Code Completion Configuations
;;

;;; Code:

(setq completion-ignore-case t)

(use-package company
  :bind (("M-/" . yas-expand)
         ("C-c C-y" . company-yasnippet)
         (:map company-active-map
               ("M-n" . nil)
               ("M-p" . nil)
               ("C-k" . company-select-previous)
               ("C-j" . company-select-next)
               ("TAB" . company-complete-common)
               ("<tab>" . company-complete-common)
               ("<backtab>" . company-select-previous))
         (:map company-search-map
               ("M-n" . nil)
               ("M-p" . nil)
               ("C-k" . company-select-previous)
               ("C-j" . company-select-next)))
  :hook (after-init . global-company-mode)
  :config
  (set-face-underline 'company-tooltip-common t)

  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-tooltip-maximum-width (/ (frame-width) 2)
        company-idle-delay 0 ; decrease delay before autocompletion popup shows
        company-echo-delay (if (display-graphic-p) nil 0) ; remove annoying blinking
        company-minimum-prefix-length 2
        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        company-dabbrev-downcase nil    ; No downcase when completion.
        company-require-match nil ; Don't require match, so you can still move your cursor as expected.
        company-backends '(company-capf company-files company-dabbrev)
        company-global-modes '(not shell-mode eshell-mode eaf-mode
                                   erc-mode message-mode help-mode
                                   helpful-mode gud-mode)
        company-format-margin-function (if (string-equal "light" (frame-parameter nil 'background-mode))
                                           #'company-vscode-light-icons-margin
                                         #'company-vscode-dark-icons-margin)
        company-icon-size '(auto-scale . 15))

  (with-eval-after-load 'company-eclim
    ;;  Stop eclim auto save.
    (setq company-eclim-auto-save nil))

  (with-eval-after-load 'company-files
    (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)")))

(use-package flx
  :defer t)

;; FIXME: unknown backend information
(use-package company-fuzzy
  :defer t
  :config
  (with-eval-after-load 'elisp-mode
    (add-hook 'emacs-lisp-mode-hook 'company-fuzzy-mode)
    (add-hook 'lisp-interaction-mode-hook 'company-fuzzy-mode)))

(use-package prescient
  :defer t
  :config
  (setq prescient-sort-length-enable nil))

(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets)
  (with-eval-after-load 'snippet
    (+funcs/major-mode-leader-keys
     snippet-mode-map
     "t" '(yas-tryout-snippet :which-key "yas-tryout-snippet"))))

;; Popup documentation for completion candidates
(use-package company-quickhelp
  :if (or (< emacs-major-version 26)
          (not (display-graphic-p)))
  :after company
  :bind ((:map company-active-map
               ("M-h" . company-quickhelp-manual-begin)))
  :hook (global-company-mode . company-quickhelp-mode)
  :config (setq company-quickhelp-delay 0.3))

(use-package company-quickhelp-terminal
  :if (not (display-graphic-p))
  :after company
  :hook (global-company-mode . company-quickhelp-terminal-mode))

(defcustom +completion/company-frontend 'company-posframe
  "Company frontend."
  :type '(choice
          (const :tag "nil" nil)
          (const :tag "company-posframe" company-posframe)
          (const :tag "company-box" company-box)))

(when (and (>= emacs-major-version 26) (display-graphic-p))
  (cond
   ((eq +completion/company-frontend 'company-posframe)
    (use-package company-posframe
      :hook (global-company-mode . company-posframe-mode)
      :bind (:map company-posframe-active-map
                  ("C-h" . company-posframe-quickhelp-toggle))
      :config
      (setq company-posframe-quickhelp-delay nil
            company-posframe-show-indicator nil
            company-posframe-show-metadata nil)
      ;; FIXME: wait upstream fix company new icon position
      (defun company-posframe-show-at-prefix (info)
        "Poshandler showing `company-posframe' at `company-prefix'."
        (let* ((parent-window (plist-get info :parent-window))
               (point (with-current-buffer (window-buffer parent-window)
                        (max (line-beginning-position)
                             (- (plist-get info :position)
                                (length company-prefix)
                                company-tooltip-margin
                                (if company-format-margin-function 1 0)))))
               (info (plist-put info :position-info (posn-at-point point parent-window))))
          (posframe-poshandler-point-bottom-left-corner info)))
      (defun +company-posframe/quickhelp-auto-hide ()
        (unless (member this-command '(mwheel-scroll
                                       handle-switch-frame
                                       company-posframe-quickhelp-scroll-up
                                       company-posframe-quickhelp-scroll-down))
          (company-posframe-quickhelp-hide)
          (remove-hook 'pre-command-hook #'+company-posframe/quickhelp-auto-hide)))

      (defun company-posframe-quickhelp-toggle ()
        (interactive)
        (if (posframe-funcall
             company-posframe-quickhelp-buffer
             (lambda ()
               (frame-parameter (window-frame) 'visibility)))
            (company-posframe-quickhelp-hide)
          (company-posframe-quickhelp-show)
          (company-posframe-quickhelp-raise-frame)
          (add-hook 'pre-command-hook #'+company-posframe/quickhelp-auto-hide 0 t)))))
   ((eq +completion/company-frontend 'company-box)
    ;; Code from https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-company.el
    (use-package company-box
      :hook (company-mode . company-box-mode)
      :config
      (setq company-box-show-single-candidate 'always
            company-box-doc-enable nil
            company-box-enable-icon t
            company-box-backends-colors nil
            company-box-highlight-prefix nil
            company-box-doc-delay 0.5
            company-box-tooltip-maximum-width company-tooltip-maximum-width
            company-box-max-candidates 50)

      (declare-function all-the-icons-faicon 'all-the-icons)
      (declare-function all-the-icons-material 'all-the-icons)
      (declare-function all-the-icons-octicon 'all-the-icons)
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
              (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
              (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
              (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
              (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
              (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
              (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
              (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
              (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
              (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
              (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
              (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
              (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
              (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
              (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
              (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
              (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
            company-box-icons-alist 'company-box-icons-all-the-icons)

      ;; Don't show documentation in echo area, because company-box displays its own
      ;; in a child frame.
      (when company-box-doc-enable
        (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends)))

      ;; "C-h" toggle doc frame manually
      (defun company-box-doc-manually-a ()
        (interactive)
        (let ((frame (or (frame-parent) (selected-frame))))
          (company-box-doc--show company-selection frame)
          (defun +company-box/auto-hide-frame-h ()
            "auto hide company-box doc frame if not scrolling the frame"
            ;; (message "log: this-command %s" this-command)
            (unless (member this-command '(mwheel-scroll handle-switch-frame ignore))
              (company-box-doc--hide frame)
              (remove-hook 'pre-command-hook #'+company-box/auto-hide-frame-h)))
          (add-hook 'pre-command-hook #'+company-box/auto-hide-frame-h)))

      (advice-add 'company-box-doc-manually :override #'company-box-doc-manually-a)))
   (t nil)))


(provide 'init-completion)

;;; init-completion.el ends here
