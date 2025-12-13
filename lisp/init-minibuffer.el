;; init-minibuffer.el --- minibuffer completions	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  minibuffer completions
;;

;;; Code:

;; Completion Styles
;; NOTE: [Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html)
;; NOTE: [Understanding Minibuffer Completion](https://www.masteringemacs.org/article/understanding-minibuffer-completion)
;; NOTE: [Emacs Minibuffer Completions](https://www.youtube.com/watch?v=w9hHMDyF9V4)
;; NOTE: [Emacs 30 Built-in Completion UI & Styles Overview](https://www.youtube.com/watch?v=-ZeoGVtMLaU)
;; NOTE: [Exploring Emacs Completion Styles - System Crafters Live!](https://www.youtube.com/watch?v=QNdOBXVdd1Q)
;; `completion-category-overrides' > `completion-category-defaults' > `completion-styles'

;; In case you like auto completion settings, where the completion popup appears immediately,
;; better use a cheap completion style like `basic', which performs prefix filtering.
(setq completion-styles '(basic)
      completion-pcm-leading-wildcard t
      completion-ignore-case t)

;; Vertico provides a performant and minimalistic vertical completion UI
;; based on the default completion system.  By reusing the built-in
;; facilities, Vertico achieves full compatibility with built-in Emacs
;; completion commands and completion tables.
(use-package vertico
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :bind ((:map vertico-map
          ("C-k" . vertico-previous)
          ("C-j" . vertico-next)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word)
          ([escape] . abort-recursive-edit)
          ("TAB" . minibuffer-complete)))
  :config
  (with-eval-after-load 'evil
    (dolist (mode '(vertico-mode
                    vertico-buffer-mode
                    vertico-flat-mode
                    vertico-grid-mode
                    vertico-indexed-mode
                    vertico-mouse-mode
                    vertico-multiform-mode
                    vertico-reverse-mode
                    vertico-unobtrusive-mode))
      (evil-set-initial-state mode 'emacs))))

(use-package vertico-posframe
  :if (or (display-graphic-p)
          (featurep 'tty-child-frames))
  :after vertico
  :config
  ;; The 'undecorated' frame parameter lets Emacs draw a border around the
  ;; child frame (default is no border). The 'tty-non-selected-cursor'
  ;; parameter makes redisplay put the terminal cursor in a non-selected
  ;; frame which is nice for things like 'consult-buffer'
  (setq vertico-posframe-parameters
        '((tty-non-selected-cursor . t)
          (title . "vertico-posframe")
          ;; (undecorated . nil)
          (left-fringe . 8)
          (right-fringe . 8)))
  ;; NOTE: vertico-posframe-mode will be activated/deactivated by `vertico-multiform-mode'
  ;; dynamically when you add ‘posframe’ setting to vertico-multiform-commands,
  ;; please do not enable vertico-posframe-mode globally at the moment.
  (setq vertico-multiform-commands '((consult-line (:not posframe))
                                     (consult-imenu (:not posframe))
                                     (consult-flymake (:not posframe))
                                     (consult-jq (:not posframe))
                                     (t posframe)))
  (vertico-multiform-mode))

;; `orderless' completion style.
(use-package orderless
  :config
  (defun customize-minibuffer-completion-styles ()
    (setq-local completion-category-defaults nil
                completion-styles '(orderless basic)))
  (add-hook 'minibuffer-setup-hook #'customize-minibuffer-completion-styles)
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; pinyin
(use-package pinyinlib
  :after orderless
  :autoload completion--regex-pinyin
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str))))

;; Helpful minibuffer annotations
(use-package marginalia
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-align 'left)
  :config
  (defun marginalia--time-absolute@advice (time)
    (let ((system-time-locale "C"))
      (format-time-string
       ;; `decoded-time-year' is only available on Emacs 27, use nth 5 here.
       (if (> (nth 5 (decode-time (current-time)))
              (nth 5 (decode-time time)))
           " %Y-%m-%d"
         "%m-%d %H:%M")
       time)))
  (advice-add 'marginalia--time-absolute :override #'marginalia--time-absolute@advice))

;; Consult implements a set of `consult-<thing>' commands, which aim to
;; improve the way you use Emacs.  The commands are founded on
;; `completing-read', which selects from a list of candidate strings.
;;
;; The Consult commands are compatible with multiple completion systems
;; based on the Emacs `completing-read' API, including the default
;; completion system, Vertico, Mct and Icomplete.
(use-package consult
  :bind ("C-s" . consult-line)
  :init
  (setq xref-show-xrefs-function 'consult-xref
        xref-show-definitions-function #'consult-xref)
  :commands (consult-buffer consult-imenu consult-line consult-grep consult-ripgrep consult--read consult-locate)
  :config
  (setq consult-preview-key 'any
        consult-async-min-input 2
        consult-line-start-from-top t
        consult-buffer-sources '(consult--source-hidden-buffer consult--source-modified-buffer consult--source-buffer)
        consult-project-buffer-sources '(+minibuffer/consult--source-project-buffer))

  ;; customize `consult--customize-alist'
  (consult-customize consult-theme :preview-key '(:debounce 0.5 any))

  ;; custom project buffer source
  (defvar +minibuffer/consult--source-project-buffer
    `(:name "Project Buffer"
      :narrow ?b
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :enabled ,(lambda () consult-project-function)
      :items
      ,(lambda ()
         (let* ((bufs (project-buffers (project-current t)))
                (filtered-bufs (cl-remove-if
                                (lambda (buffer)
                                  (project--buffer-check buffer project-ignore-buffer-conditions))
                                bufs))
                (buffer-pairs (mapcar (lambda (buffer) (cons (buffer-name buffer) buffer)) filtered-bufs)))
           buffer-pairs)))
    "Project buffer source for `consult-buffer'."))

(use-package consult-dir
  :defer t
  :bind ((:map vertico-map
          ("C-x C-d" . consult-dir)))
  :config
  (setq consult-dir-default-command #'consult-dir-dired)

  (defun consult-dir--zoxide-dirs ()
    "Return list of zoxide dirs."
    (split-string (shell-command-to-string "zoxide query -l") "\n" t))

  (defvar consult-dir--source-zoxide
    `(:name "zoxide"
      :narrow ?z
      :category file
      :face consult-file
      :history file-name-history
      :enabled ,(lambda () (executable-find "zoxide"))
      :items ,#'consult-dir--zoxide-dirs)
    "zoxide directory source for `consult-dir'.")

  ;; [More directory sources](https://github.com/karthink/consult-dir?tab=readme-ov-file#directory-sources)
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t))

;; Navigate the Xref stack with Consult.
(use-package consult-xref-stack
  :vc (:url "https://github.com/brett-lempereur/consult-xref-stack")
  :bind (("C-," . consult-xref-stack-backward)))

;; Emacs Mini-Buffer Actions Rooted in Keymaps
;; NOTE: https://karthinks.com/software/fifteen-ways-to-use-embark/
;;
;; Embark is an Emacs package that acts like a context menu, allowing
;; users to perform context-sensitive actions on selected items
;; directly from the completion interface.
(use-package embark
  :defer t
  :bind
  (("C-." . embark-act) ;; pick some comfortable binding
   ("C-;" . embark-dwim) ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (with-eval-after-load 'evil
    (evil-global-set-key 'normal (kbd "C-.") #'embark-act)
    (evil-global-set-key 'normal (kbd "C-;") #'embark-dwim))
  :config
  (with-eval-after-load 'which-key
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators
          '(embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))

    (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator))

  (define-key embark-file-map (kbd "S") '+funcs/sudo-find-file))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-mode))


(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
