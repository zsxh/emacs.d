;; eglot-codelens-x.el --- Add support for codelenses to eglot -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'jsonrpc)
(require 'eglot)

(require 'nerd-icons nil t)


;;; Codelens

(defvar-local eglot--outstanding-codelens-region (cons nil nil)
  "Jit-lock-calculated (FROM . TO) region with potentially outdated codelens.")

(defvar-local eglot--outstanding-codelens-last-region nil)

(defvar-local eglot--outstanding-codelens-regions-timer nil
  "Helper timer for `eglot--update-codelens'.")

(cl-defun eglot--update-codelens (from to)
  "Jit-lock function for Eglot codelens."
  ;; XXX: We're relying on knowledge of jit-lock internals here.
  ;; Comparing `jit-lock-context-unfontify-pos' (if non-nil) to
  ;; `point-max' tells us whether this call to `jit-lock-functions'
  ;; happens after `jit-lock-context-timer' has just run.
  (when (and jit-lock-context-unfontify-pos
             (/= jit-lock-context-unfontify-pos (point-max)))
    (cl-return-from eglot--update-codelens))
  (cl-symbol-macrolet ((region eglot--outstanding-codelens-region)
                       (last-region eglot--outstanding-codelens-last-region)
                       (timer eglot--outstanding-codelens-regions-timer))
    (setcar region (min (or (car region) (point-max)) from))
    (setcdr region (max (or (cdr region) (point-min)) to))
    ;; XXX: Then there is a smoothing timer.  I wish we didn't need it,
    ;; but sometimes a lot of calls come in all at once and do make it
    ;; past the check above.  Notice it is a 0 second timer though, so
    ;; we're not introducing any more delay over jit-lock's timers.
    (when timer (cancel-timer timer))
    (setq timer (run-at-time
                 0 nil
                 (lambda (buf)
                   (eglot--when-live-buffer buf
                     ;; HACK: In some pathological situations
                     ;; (Emacs's own coding.c, for example),
                     ;; jit-lock is calling `eglot--update-codelens'
                     ;; repeatedly with same sequence of
                     ;; arguments, which leads to
                     ;; `eglot--update-codelens-1' being called with
                     ;; the same region repeatedly.  This happens
                     ;; even if the codelens-painting code does
                     ;; nothing else other than widen, narrow,
                     ;; move point then restore these things.
                     ;; Possible Emacs bug, but this fixes it.
                     (unless (equal last-region region)
                       (eglot--update-codelens-1 (max (car region) (point-min))
                                                 (min (cdr region) (point-max)))
                       (setq last-region region))
                     (setq region (cons nil nil)
                           timer nil)))
                 (current-buffer)))))

(defun paint-line-codelens (codelens)
  (let* ((region-codelens (codelens-in-region codelens))
         (line-codelens (codelens-group-by-line)))
    (dolist (line-codelens line-code-lenses)
      (let* ((cls (cdr line-codelens))
             (size (length cls)))
        (seq-map-indexed
         (lambda (lens idx)
           (eglot-codelens-make-overlay-for-lens
            (or (plist-get lens :command)
                (plist-get (jsonrpc-request
                            (eglot--current-server-or-lose)
                            :codeLens/resolve
                            lens)
                           :command))
            (plist-get lens :range)
            idx
            (= idx (- size 1))))
         cls)))))

(defun eglot-codelens-in-region (codelens from to)
  (cl-remove-if
   (lambda (codelen)
     (let* ((range (plist-get codelen :range))
            (start (plist-get range :start))
            (p (eglot--lsp-position-to-point start)))
       (or (> p to) (< p from))))
   codelens))

(defun eglot-codelens-group-by-line (codelens)
  (seq-group-by
   (lambda (codelen)
     (let* ((range (plist-get codelen :range))
            (start (plist-get range :start))
            (line (plist-get start :line)))
       line))
   codelens))

(defun eglot-codelens-paint-line-codelens (codelens from to)
  (let* ((region-codelens (eglot-codelens-in-region codelens from to))
         (line-codelens-group (eglot-codelens-group-by-line region-codelens)))
    (dolist (line-codelens line-codelens-group)
      (let* ((cls (cdr line-codelens))
             (size (length cls)))
        (seq-map-indexed
         (lambda (lens idx)
           (eglot-codelens-make-overlay-for-lens
            (or (plist-get lens :command)
                (plist-get (jsonrpc-request
                            (eglot--current-server-or-lose)
                            :codeLens/resolve
                            lens)
                           :command))
            (plist-get lens :range)
            idx
            (= idx (- size 1))))
         cls)))))

(defun eglot--update-codelens-1 (from to)
  "Do most work for `eglot--update-codelens', including LSP request."
  (let* ((buf (current-buffer)))
    (jsonrpc-async-request
     (eglot--current-server-or-lose)
     :textDocument/codeLens
     (list :textDocument (eglot--TextDocumentIdentifier))
     :success-fn (lambda (codelens)
                   (eglot--when-live-buffer buf
                     (eglot--widening
                      ;; Overlays ending right at FROM with an
                      ;; `after-string' property logically belong to
                      ;; the (FROM TO) region.  Likewise, such
                      ;; overlays ending at TO don't logically belong
                      ;; to it.
                      (dolist (o (overlays-in (1- from) to))
                        (when (and (overlay-get o 'eglot--codelens)
                                   (cond ((eq (overlay-end o) from)
                                          (overlay-get o 'after-string))
                                         ((eq (overlay-end o) to)
                                          (overlay-get o 'before-string))
                                         (t)))
                          (delete-overlay o)))
                      (eglot-codelens-paint-line-codelens codelens from to))))
     :deferred 'eglot--update-codelens-1)))

(defun eglot-codelens-make-overlay-for-lens (command range priority last-elt-p)
  "Insert overlays for each corresponding lens's COMMAND and RANGE."
  (let* ((start-line (thread-first range
                                   (plist-get :start)
                                   (plist-get :line)))
         (bol-pos-and-indent-str
          (eglot-codelens-overlay-pos-and-indent-str start-line))
         (bol-pos (car bol-pos-and-indent-str))
         (indent-str (cdr bol-pos-and-indent-str))
         (ol (make-overlay bol-pos (1+ bol-pos)))
         (text (concat
                (when (eq 0 priority)
                  indent-str)
                (propertize
                 (eglot-codelens-codicons-to-nerd-icons
                  (plist-get command :title))
                 'face 'eglot-parameter-hint-face
                 'cursor t
                 'pointer 'hand
                 'mouse-face 'highlight
                 'keymap (let ((map (make-sparse-keymap)))
                           (define-key map [mouse-1]
                             (lambda () (interactive)
                               (eglot-codelens-execute command)))
                           map))
                (if last-elt-p
                    "\n"
                  (propertize "|" 'face 'eglot-parameter-hint-face)))))
    ;; Try to combine all the cls into a single overlay so we can
    ;; use this text property to prevent the cursor from ending up on
    ;; the right side of the overlay
    ;; taken from [[file:~/.emacs.d/elpa/flymake-1.3.7/flymake.el::put-text-property 0 1 'cursor t summary][eol overlays from flymake]]
    (put-text-property 0 1 'cursor t text)
    (overlay-put ol 'before-string text)
    (overlay-put ol 'eglot-codelens-command (list :command command
                                                  :range range))
    (overlay-put ol 'cursor-face 'error)
    (overlay-put ol 'priority priority)
    (overlay-put ol 'eglot--codelens t)
    (overlay-put ol 'eglot--overlay t)))

(defun eglot-codelens-overlay-pos-and-indent-str (start-line)
  (let ((indent-str nil)
        (bol-pos nil))
    (save-excursion
      (goto-char (point-min))
      (forward-line start-line)
      (beginning-of-line)
      (setq bol-pos (point))
      (if (looking-at "\s*")
          (setq indent-str (match-string 0))
        (setq indent-str "")))
    (cons bol-pos indent-str)))

(defun eglot-codelens-codicons-to-nerd-icons (title)
  "Convert VS Code icon placeholders in TITLE to nerd icons.
VS Code icon placeholder syntax: $(icon-name)
Converts to nerd icons using nerd-icons-codicon.
If the icon is not recognized, returns the original placeholder."
  (if (featurep 'nerd-icons)
      (replace-regexp-in-string
       "\\$(\\([^)]+\\))"
       (lambda (match)
         (let* ((icon-name (match-string 1 match))
                (nerd-icon-name (replace-regexp-in-string "-" "_" icon-name))
                (icon-code (format "nf-cod-%s" nerd-icon-name)))
           (condition-case err
               (nerd-icons-codicon icon-code)
             (error
              ;; If nerd-icons-codicon fails, return the original placeholder
              match))))
       title
       nil
       t)
    title))

(defun eglot-codelens-execute (command)
  "Execute a the COMMAND for a specific Code Lens."
  (eglot-execute (eglot--current-server-or-lose)
                 command))

(define-minor-mode eglot-codelens-mode
  "Minor mode for displaying Code Lenses with LSP server's codlens."
  :global nil
  (cond (eglot-codelens-mode
         (if (and (eglot-managed-p)
                  (eglot-server-capable :codeLensProvider))
             (jit-lock-register #'eglot--update-codelens 'contextual)
           (eglot-codelens-mode -1)))
        (t
         (eglot--delete-overlays 'eglot--codelens))))


(provide 'eglot-codelens-x)
;;; eglot-codelens-x.el ends here
