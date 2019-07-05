;; init-prog.el --- Common Programing Settings	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  Common Programing Settings
;;

;;; Code:

;;;;;;;;;;;;;; Quick Scroll line ;;;;;;;;;;;;;;
;; keymap ("C-l" 'recenter-top-bottom) cycling 25%,top,bottom line position
(add-hook 'prog-mode-hook (lambda () (setq-local recenter-positions '(0.25 top bottom))))

;;;;;;;;;;;;;; VIEW ;;;;;;;;;;;;;;

;; Focus provides focus-mode that dims the text of surrounding sections
;; https://github.com/larstvei/Focus
(use-package focus
  :ensure t
  :commands (focus-mode focus-read-only-mode)
  :config
  (with-eval-after-load 'lsp-mode
    (add-to-list 'focus-mode-to-thing '(lsp-mode . lsp-folding-range))))

;;;;;;;;;;;;;; DOC ;;;;;;;;;;;;;;

;; require `zeal' installation
;; zeal (dash for linux)
(use-package zeal-at-point
  :ensure t
  :commands zeal-at-point)

;; require `zeal' or `dash' docsets
;; helm-dash
(use-package helm-dash
  :ensure t
  :commands (helm-dash helm-dash-at-point)
  :config
  ;; (setq helm-dash-browser-func 'eww)
  (setq helm-dash-browser-func 'eaf-open-url)
  (setq helm-dash-docsets-path (expand-file-name "~/.local/share/Zeal/Zeal/docsets"))
  (setq helm-dash-common-docsets (dash-docs-installed-docsets))
  (setq helm-dash-enable-debugging nil))

;;;;;;;;;;;;;; EDIT ;;;;;;;;;;;;;;

;; automatic parenthesis pairing for non prog mode
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

(use-package awesome-pair
  :commands awesome-pair-mode
  :hook ((prog-mode conf-mode yaml-mode editorconfig-mode) . awesome-pair-mode)
  :bind (:map
         awesome-pair-mode-map
         ("(" . 'awesome-pair-open-round)
         ("[" . 'awesome-pair-open-bracket)
         ("{" . 'awesome-pair-open-curly)
         (")" . 'awesome-pair-close-round)
         ("]" . 'awesome-pair-close-bracket)
         ("}" . 'awesome-pair-close-curly)
         ("%" . 'awesome-pair-match-paren)
         ("\"" . 'awesome-pair-double-quote)
         ("M-o" . 'awesome-pair-backward-delete)
         ("DEL" . 'awesome-pair-backward-delete)
         ("C-d" . 'awesome-pair-forward-delete)
         ("C-k" . 'awesome-pair-kill)
         ("M-\"" . 'awesome-pair-wrap-double-quote)
         ("M-[" . 'awesome-pair-wrap-bracket)
         ("M-{" . 'awesome-pair-wrap-curly)
         ("M-(" . 'awesome-pair-wrap-round)
         ("M-]" . 'awesome-pair-unwrap)
         ("M-n" . 'awesome-pair-jump-right)
         ("M-p" . 'awesome-pair-jump-left)
         ("M-RET" . 'awesome-pair-jump-out-pair-and-newline))
  :config
  (defun awesome-pair-in-string-p (&optional state)
    (save-excursion
      (or (nth 3 (or state (awesome-pair-current-parse-state)))
          (and
           (eq (get-text-property (point) 'face) 'font-lock-string-face)
           (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
          (and
           (eq (get-text-property (point) 'face) 'font-lock-doc-face)
           (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
          ;; fix single quote delete for c/c++/java-mode
          (and
           (eq ?\" (char-syntax (char-before)))
           (eq ?\" (char-syntax (char-after (point))))))))

  (require 'paren)

  (defun +prog/backward-char-pair-p ()
    (save-excursion
      (backward-char)
      (show-paren--default)))

  (defun +prog/forward-char-pair-p ()
    (save-excursion
      (forward-char)
      (show-paren--default)))

  ;; For rust-mode Angle brackets
  (defun awesome-pair-after-open-pair-p ()
    (save-excursion
      (let ((syn (char-syntax (char-before))))
        (and
         (or (eq syn ?\()
             (and (eq syn ?_)
                  (eq (char-before) ?\{)))
         (+prog/backward-char-pair-p)))))

  (defun awesome-pair-after-close-pair-p ()
    (save-excursion
      (let ((syn (char-syntax (char-before))))
        (and
         (or (eq syn ?\))
             (eq syn ?\")
             (and (eq syn ?_)
                  (eq (char-before) ?\})))
         (+prog/backward-char-pair-p)))))

  (defun awesome-pair-before-open-pair-p ()
    (save-excursion
      (let ((syn (char-syntax (char-after))))
        (and
         (or (eq syn ?\( )
             (eq syn ?\" )
             (and (eq syn ?_)
                  (eq (char-after) ?\{)))
         (+prog/forward-char-pair-p)))))

  (defun awesome-pair-before-close-pair-p ()
    (save-excursion
      (let ((syn (char-syntax (char-after))))
        (and
         (or (eq syn ?\) )
             (and (eq syn ?_)
                  (eq (char-after) ?\})))
         (+prog/forward-char-pair-p)))))

  ;; (with-eval-after-load 'cc-mode
  ;;   (modify-syntax-entry ?< "(>" c++-mode-syntax-table)
  ;;   (modify-syntax-entry ?> ")<" c++-mode-syntax-table)
  ;;   (modify-syntax-entry ?< "(>" java-mode-syntax-table)
  ;;   (modify-syntax-entry ?> ")<" java-mode-syntax-table))

  (with-eval-after-load 'lispy
    (define-key lispy-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
    (define-key lispy-mode-map (kbd "M-n") 'awesome-pair-jump-right)
    (define-key lispy-mode-map (kbd "M-p") 'awesome-pair-jump-left)
    (define-key lispy-mode-map (kbd "M-RET") 'awesome-pair-jump-out-pair-and-newline)))

;; Short and sweet LISP editing
(use-package lispy
  :ensure t
  :commands lispy-mode
  :hook ((emacs-lisp-mode . (lambda () (lispy-mode 1)))
         (lisp-interaction-mode . (lambda () (lispy-mode 1)))
         (lisp-mode . (lambda () (lispy-mode 1))))
  :config
  (require 'le-lisp)
  (setq lispy-use-sly t)

  ;; Replace lispy--eavl-lisp function
  (defun lispy--eval-lisp-advice (str)
    "Eval STR as Common Lisp code."
    (let* ((deactivate-mark nil)
           (result (with-current-buffer (process-buffer (lispy--cl-process))
                     (if lispy-use-sly
                         (sly-interactive-eval str)
                       (slime-eval `(swank:eval-and-grab-output ,str))))))
      (if (equal (car result) "")
          (cadr result)
        (concat (propertize (car result)
                            'face 'font-lock-string-face)
                "\n\n"
                (cadr result)))))
  (advice-add #'lispy--eval-lisp :override #'lispy--eval-lisp-advice))

;; Change variable name style
(use-package string-inflection
  :ensure t
  :commands string-inflection-all-cycle)

;;;;;;;;;;;;;; Code Folding ;;;;;;;;;;;;;;
;; evil open/close/toggle folds rely on hideshow
;; "z a" evil-toggle-fold
;; "z m" evil-close-folds
;; "z r" evil-open-folds
(use-package hideshow
  :commands hs-minor-mode
  ;; FIXME: https://github.com/millejoh/emacs-ipython-notebook/issues/464#issuecomment-460380151
  ;; the bicycle extension was using hideshow and conflicted with ein, so don't enable hs-minor-mode for prog-mode
  :hook ((java-mode rust-mode python-mode) . (lambda () (hs-minor-mode) (hs-hide-level 2)))
  :config
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "z f") 'hs-hide-level)))

;; https://gitlab.com/jgkamat/rmsbolt
;; RMSBolt tries to make it easy to see what your compiler is doing.
;; It does this by showing you the assembly output of a given source code file.
(use-package rmsbolt
  :ensure t
  :commands rmsbolt-mode)

;; Evil shift indent
(defvar prog--indent-variable-alist
  '(((awk-mode c-mode c++-mode java-mode groovy-mode
               idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
    (python-mode . python-indent-offset)
    (cmake-mode . cmake-tab-width)
    (coffee-mode . coffee-tab-width)
    (cperl-mode . cperl-indent-level)
    (css-mode . css-indent-offset)
    (elixir-mode . elixir-smie-indent-basic)
    ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
    ((js-mode json-mode) . js-indent-level)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)
    (latex-mode . (LaTeX-indent-level tex-indent-basic))
    (livescript-mode . livescript-tab-width)
    (mustache-mode . mustache-basic-offset)
    (nxml-mode . nxml-child-indent)
    (perl-mode . perl-indent-level)
    (puppet-mode . puppet-indent-level)
    (ruby-mode . ruby-indent-level)
    (scala-mode . scala-indent:step)
    (sgml-mode . sgml-basic-offset)
    (sh-mode . sh-basic-offset)
    (web-mode . web-mode-markup-indent-offset)
    (yaml-mode . yaml-indent-offset))
  "An alist where each key is either a symbol corresponding\
to a major mode, a list of such symbols, or the symbol t,
acting as default. The values are either integers, symbols
or lists of these.")

(defun +prog/set-evil-shift-width ()
  "Set the value of `evil-shift-width' based on the indentation settings of the\
current major mode."
  (let ((shift-width
         (catch 'break
           (dolist (test prog--indent-variable-alist)
             (let ((mode (car test))
                   (val (cdr test)))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break (default-value 'evil-shift-width)))))
    (when (and (integerp shift-width)
               (< 0 shift-width))
      (setq-local evil-shift-width shift-width))))

(add-hook 'after-change-major-mode-hook '+prog/set-evil-shift-width)

(setq-default evil-shift-width 2)

;;;;;;;;;;;;;; Coding styles for multiple developers working on the same project across various editors and IDEs ;;;;;;;;;;;;;;

(use-package editorconfig
  :ensure t
  :hook (emacs-startup . editorconfig-mode))

(provide 'init-prog)


;;; init-prog.el ends here
