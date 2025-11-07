;; init-ai.el --- AI packages	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  AI packages
;;

;;; Code:

;; Embark Actions
(with-eval-after-load 'embark
  (keymap-set embark-general-map "?" #'gptel-quick)
  (keymap-set embark-general-map "." #'gptel-commit))

;; A simple ChatGPT client for Emacs
;; https://github.com/karthink/gptel
;; NOTE: https://github.com/karthink/gptel/wiki#defining-custom-gptel-commands
;; NOTE: https://ollama.com/blog/how-to-prompt-code-llama
(use-package gptel
  :defer t
  :bind (("<f8>" . gptel)
         :map gptel-mode-map
         ("C-c h" . gptel-menu)
         ("C-c C-l" . my/gptel-insert))
  :config
  (require 'gptel-integrations)
  (setq gptel-proxy (format "http://%s:%s" personal-proxy-http-host personal-proxy-http-port)
        gptel-expert-commands t
        gptel-log-level 'debug
        gptel-temperature 1.0
        gptel-include-reasoning 'ignore
        gptel-track-media t
        gptel-use-context 'system)

  (setf (alist-get 'markdown-mode gptel-prompt-prefix-alist) "user  ")
  (setf (alist-get 'markdown-mode gptel-response-prefix-alist) "assistant \n")

  (define-minor-mode gptel-highlight-mode
    "Visually highlight LLM respones regions.

Highlighting is via fringe or margin markers, and optionally a response
face.  See `gptel-highlight-methods' for highlighting methods, and
`gptel-response-highlight' and `gptel-response-fringe-highlight' for the
faces.

This minor mode can be used anywhere in Emacs, and not just gptel chat
buffers."
    :lighter nil
    :global nil
    (cond
     (gptel-highlight-mode
      (when (memq 'margin gptel-highlight-methods)
        (setq left-margin-width (1+ left-margin-width))
        ;; HACK: Only refresh window margin when gptel-mode is active in current window
        (when (buffer-local-value 'gptel-mode (window-buffer))
          (set-window-buffer (selected-window) (current-buffer))))
      (jit-lock-register #'gptel-highlight--update)
      (gptel-highlight--update (point-min) (point-max)))
     (t (when (memq 'margin gptel-highlight-methods)
          (setq left-margin-width (max (1- left-margin-width) 0))
          (set-window-buffer (selected-window) (current-buffer)))
        (jit-lock-unregister #'gptel-highlight--update)
        (without-restriction
          (remove-overlays nil nil 'gptel-highlight t)))))

  (add-hook 'gptel-mode-hook (lambda ()
                               ;; (make-variable-buffer-local 'gptel-context)
                               (gptel-highlight-mode)
                               (turn-on-visual-line-mode)))

  ;; NOTE: https://github.com/karthink/gptel/issues/481#issuecomment-3203716169
  (progn
    ;; Add a new prompt transformation that looks for @buffer
    (add-hook 'gptel-prompt-transform-functions 'my/gptel-inject-buffers)

    ;; The transformation:
    (defun my/gptel-inject-buffers (_)
      "Search backward, injecting text files into context as needed.

Include buffers by name as:

@buffer *scratch*"
      (while (and (re-search-backward "^\\s-*@buffer\\b" nil t) ;look for @buffer
                  (not (get-char-property (point) 'gptel))) ;avoid LLM response regions
        (goto-char (match-end 0))
        (delete-region (point) (line-beginning-position))
        (let ((buf-name (string-trim
                         (buffer-substring-no-properties
                          (point) (line-end-position)))))
          (if (not (buffer-live-p (get-buffer buf-name)))
              (message "Buffer \"%s\" not live, ignoring @buffer"
                       buf-name)
            (delete-region (point) (line-end-position))
            (insert (format "\nIn buffer `%s`:\n\n```\n" buf-name))
            (insert-buffer-substring-no-properties buf-name)
            (insert "\n```\n")))))

    (defun my/gptel-insert-buffer (&optional buffer-name)
      "Insert buffer name with @buffer prefix in current buffer.
When called interactively, prompts for buffer name with completion."
      (interactive
       (list (completing-read "Select buffer: "
                              (mapcar #'buffer-name
                                      (seq-filter
                                       (lambda (buf)
                                         (not (string-prefix-p " " (buffer-name buf))))
                                       (buffer-list)))
                              nil t nil nil (buffer-name))))
      (insert "\n@buffer " buffer-name "\n"))

    (defun my/gptel-insert-file (&optional file-path)
      "Insert a markdown file link at point.
FILE-PATH: Path to the file to link to."
      (interactive
       (list (read-file-name "Select file: ")))
      (insert " [](" file-path ") "))

    (defun my/gptel-insert (&optional type)
      "Insert file or buffer reference with appropriate prefix.
When called interactively, prompts for file or buffer type."
      (interactive
       (list (completing-read "File Or Buffer:  " '(file buffer) nil t)))
      (cond
       ((string= type "file")
        (call-interactively #'my/gptel-insert-file))
       ((string= type "buffer")
        (call-interactively #'my/gptel-insert-buffer)))))

  ;; Clean Up default backends
  (setq gptel--known-backends nil)

  ;; Customize Prompts
  (require 'gptel-prompts)
  (setq gptel-directives `((no-system-prompt . nil)
                           (code-review . ,gptel-prompt-code-review)
                           (code-explain . ,gptel-prompt-code-explain)
                           (code-refactor . ,gptel-prompt-code-refactor)
                           (readability-enhance . ,gptel-prompt-readability-enhance)
                           (ask-ai . ,gptel-prompt-ask-ai)
                           (correctness-check . ,gptel-prompt-correctness-check)
                           (prompt-enhance . ,gptel-prompt-prompt-enhance)
                           (费曼学习 . ,gptel-prompt-费曼学习)
                           (深度需求挖掘 . ,gptel-prompt-深度需求挖掘)
                           (字幕->纠错转写 . ,gptel-prompt-字幕->纠错转写)
                           (字幕->文章 . ,gptel-prompt-字幕->文章)
                           (字幕->结构化文章 . ,gptel-prompt-字幕->结构化文章)
                           (GEO-让大模型更容易推荐你的内容或产品 . ,gptel-prompt-GEO-让大模型更容易推荐你的内容或产品)
                           (动机分析与手段识别 . ,gptel-prompt-动机分析与手段识别)
                           (真实创业模拟器 . ,gptel-prompt-真实创业模拟器)
                           (导师贴心建议 . ,gptel-prompt-导师贴心建议)
                           (学术写作改 . ,gptel-prompt-学术写作改写)
                           (业务prompt撰写专家 . ,gptel-prompt-业务prompt撰写专家)
                           (基于炸弹项圈理论评估产品风险 . ,gptel-prompt-基于炸弹项圈理论评估产品风险))
        gptel--system-message (alist-get '深度需求挖掘 gptel-directives))
  ;; (add-to-list 'gptel-directives `(default . ,(alist-get '深度需求挖掘 gptel-directives)))

  ;; LLM Providers
  ;; OpenRouter
  (defvar gptel--openrouter
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key 'gptel-api-key
      :models '((anthropic/claude-sonnet-4.5
                 :capabilities (media tool-use cache)
                 :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf")
                 :context-window 200
                 :input-cost 21.4
                 :output-cost 106.82
                 :reqest-params (:provider (:only ["anthropic" "google-vertex/global" "amazon-bedrock"])))
                (google/gemini-2.5-flash
                 :capabilities (tool-use json media audio video)
                 :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                              "application/pdf" "text/plain" "text/csv" "text/html"
                              "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                              "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
                 :context-window 1048
                 :input-cost 2.14
                 :output-cost 17.83)
                (google/gemini-2.5-pro
                 :capabilities (tool-use json media audio video)
                 :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
                              "application/pdf" "text/plain" "text/csv" "text/html"
                              "audio/mpeg" "audio/wav" "audio/ogg" "audio/flac" "audio/aac" "audio/mp3"
                              "video/mp4" "video/mpeg" "video/avi" "video/quicktime" "video/webm")
                 :context-window 200
                 :input-cost 8.92
                 :output-cost 71.34)
                (openai/gpt-4.1-mini
                 :capabilities (media tool-use json url)
                 :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                 :context-window 1024
                 :input-cost 2.85
                 :output-cost 11.41)
                (openai/gpt-5
                 :capabilities (media tool-use json url)
                 :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                 :context-window 400
                 :input-cost 8.92
                 :output-cost 71.34)
                (x-ai/grok-4-fast
                 :capabilities (tool-use json reasoning)
                 :context-window 128
                 :input-cost 1.4
                 :output-cost 3.5))))

  ;; DeepSeek
  (defvar gptel--deepseek
    (gptel-make-deepseek "DeepSeek"
      :stream t
      :key 'gptel-api-key
      :models '((deepseek-chat
                 :capabilities (tool-use)
                 :context-window 128
                 :input-cost 2
                 :output-cost 3)
                (deepseek-reasoner
                 :capabilities (reasoning tool-use)
                 :context-window 128
                 :input-cost 2
                 :output-cost 3))))

  ;; GLM
  (defvar gptel--glm
    (gptel-make-openai "GLM"
      :host "open.bigmodel.cn"
      :endpoint "/api/paas/v4/chat/completions"
      :stream t
      :key 'gptel-api-key
      :models '((glm-4.6
                 :request-params (:thinking (:type "disabled"))
                 :capabilities (tool-use reasoning)
                 :context-window 200
                 :input-cost 4
                 :output-cost 16))))

  ;; MiniMax
  (defvar gptel--minimax
    (gptel-make-anthropic "MiniMax"
      :host "api.minimaxi.com"
      :endpoint "/anthropic/v1/messages"
      :stream t
      :key 'gptel-api-key
      :models '((MiniMax-M2
                 :request-params (:temperature 1.0
                                  :top_p 0.95
                                  :top_k 20)
                 :capabilities (tool-use reasoning)
                 :context-window 200
                 :input-cost 2.1
                 :output-cost 8.4))))

  ;; Siliconflow
  (defvar gptel--siliconflow
    (gptel-make-openai "Siliconflow"
      :host "api.siliconflow.cn"
      :stream t
      :key 'gptel-api-key
      :models '((Pro/deepseek-ai/DeepSeek-V3.2-Exp
                 :request-params (:enable_thinking :json-false)
                 :capabilities (tool-use reasoning)
                 :context-window 160
                 :input-cost 2
                 :output-cost 3)
                (deepseek-ai/DeepSeek-OCR
                 :capabilities (media tool-use json url)
                 :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
                 :context-window 8
                 :input-cost 0
                 :output-cost 0)
                (Pro/moonshotai/Kimi-K2-Instruct-0905
                 :request-params (:temperature 0.6)
                 :capabilities (tool-use)
                 :context-window 256
                 :input-cost 4
                 :output-cost 16)
                (Qwen/Qwen3-235B-A22B-Instruct-2507
                 :request-params (:temperature 0.7
                                  :top_p 0.8
                                  :top_k 20
                                  :min_p 0)
                 :capabilities (tool-use)
                 :context-window 256
                 :input-cost 2.5
                 :output-cost 10)
                (zai-org/GLM-4.6
                 :request-params (:enable_thinking :json-false)
                 :capabilities (tool-use reasoning)
                 :context-window 198
                 :input-cost 3.5
                 :output-cost 14)
                (MiniMaxAI/MiniMax-M2
                 :request-params (:temperature 1.0
                                  :top_p 0.95
                                  :top_k 20)
                 :capabilities (tool-use reasoning)
                 :context-window 192
                 :input-cost 2.1
                 :output-cost 8.4))))

  ;; default model
  (setq gptel-backend gptel--deepseek
        gptel-model 'deepseek-chat))

(use-package mcp
  :defer t
  :config
  (require 'mcp-hub)
  (exec-path-from-shell-copy-envs
   '("CONTEXT7_API_KEY" "EXA_API_KEY" "METASO_API_KEY" "TAVILY_API_KEY" "GITHUB_API_TOKEN"))

  (setq mcp-hub-servers
        `(("tavily" . (:url ,(format "https://mcp.tavily.com/mcp/?tavilyApiKey=%s" (getenv "TAVILY_API_KEY"))))
          ("context7" . (:url "https://mcp.context7.com/mcp"
                         :headers (("CONTEXT7_API_KEY" . ,(getenv "CONTEXT7_API_KEY")))))
          ("github" . (:url "https://api.githubcopilot.com/mcp/"
                       :headers (("Authorization" . ,(getenv "GITHUB_API_TOKEN")))))
          ;; ("metaso" . (:url "https://metaso.cn/api/mcp"
          ;;              :headers (("Authorization" . ,(format "Bearer %s" (getenv "METASO_API_KEY"))))))
          ("exa" . (:url ,(format "https://mcp.exa.ai/mcp?exaApiKey=%s" (getenv "EXA_API_KEY"))))
          ("searxng" . (:command "npx"
                        :args ("-y" "mcp-searxng")
                        :env (:SEARXNG_URL "http://localhost:8888")))))

  (with-eval-after-load 'evil
    (evil-define-key* '(normal visual) mcp-hub-mode-map
      "d" #'mcp-hub-close-server
      "j" #'next-line
      "k" #'previous-line
      "l" #'mcp-hub-view-log
      "r" #'mcp-hub-restart-server
      "s" #'mcp-hub-start-server
      "D" #'mcp-hub-close-all-server
      "R" #'mcp-hub-restart-all-server
      "S" #'mcp-hub-start-all-server)))

;; transient keymap
;; - `+': `more-response'
;; - `M-w': `copy-response'
;; - `M-RET': `create-chat'
(use-package gptel-quick
  :vc (:url "https://github.com/karthink/gptel-quick")
  :defer t
  :config
  (setq gptel-quick-system-message
        (lambda (count)
          (format "Explain in %d words or fewer in chinese." count))
        gptel-quick-timeout nil))

(use-package gptel-commit
  :ensure nil
  :commands (gptel-commit))

;; An AI Writing Assistant for Emacs
;; https://github.com/dolmens/gptel-aibo
(use-package gptel-aibo
  :defer t
  :bind (("C-c i" . gptel-aibo-summon)
         (:map gptel-aibo-mode-map
          ("C-c !" . gptel-aibo-apply-last-suggestions))))

;; https://github.com/milanglacier/minuet-ai.el
(use-package minuet
  :defer t
  :bind (("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
         :map minuet-active-mode-map
         ("<tab>" . #'minuet-accept-suggestion)
         ("TAB" . #'minuet-accept-suggestion))
  :config
  (require 'gptel)
  (setq minuet-provider 'openai-compatible
        minuet-n-completions 1
        minuet-request-timeout 2.5
        minuet-auto-suggestion-throttle-delay 1.0
        minuet-auto-suggestion-debounce-delay 0.4)

  (plist-put minuet-openai-compatible-options :end-point "https://api.deepseek.com/chat/completions")
  (plist-put minuet-openai-compatible-options :api-key (lambda () (gptel-api-key-from-auth-source "api.deepseek.com")))
  (plist-put minuet-openai-compatible-options :model "deepseek-chat")

  ;; (plist-put minuet-openai-compatible-options :end-point "https://openrouter.ai/api/v1/chat/completions")
  ;; (plist-put minuet-openai-compatible-options :api-key (lambda () (gptel-api-key-from-auth-source "openrouter.ai")))
  ;; (plist-put minuet-openai-compatible-options :model "google/gemini-2.5-flash-lite")
  ;; (plist-put minuet-openai-compatible-options :model "x-ai/grok-code-fast-1")
  ;; (plist-put minuet-openai-compatible-options :model "x-ai/grok-4-fast")

  ;; Prioritize throughput for faster completion
  ;; (minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 256)
  (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)
  )

;; use `whisper-cpp-download-ggml-model' from nixpkgs.whisper-cpp
;; > whisper-cpp-download-ggml-model {model} {whisper-install-directory}/whisper.cpp/models
;;
;; use `huggingface-cli'
;; > huggingface-cli download --resume-download {model} --local-dir {model-repo-dir}
;;
;; NOTE: MacOS Configuration Requirements, https://github.com/natrys/whisper.el/wiki/MacOS-Configuration
;; - Grant Emacs permission to use Mic
;;   - Reset it: tccutil reset Microphone org.gnu.Emacs
;; - Set whisper--ffmpeg-input-device
(use-package whisper
  :if (executable-find "whisper-cli")
  :vc (:url "https://github.com/natrys/whisper.el.git")
  :commands (whisper-run whisper-file)
  :bind (("C-c r" . whisper-run))
  :config
  (setq whisper-install-whispercpp nil
        whisper-install-directory (locate-user-emacs-file "cache")
        ;; whisper-model "small"
        whisper-model "large-v3-turbo"
        ;; whisper-quantize "q5_0"
        whisper-quantize nil
        whisper-language "auto"
        whisper-translate nil
        whisper-use-threads (/ (num-processors) 2))

  ;; Prompt for whisper transcription
  ;; https://github.com/grapeot/brainwave/blob/master/prompts.py
  (setq whisper-prompt "Comprehend the accompanying audio, and output the recognized text. You may correct any grammar and punctuation errors, but don't change the meaning of the text. You can add bullet points and lists, but only do it when obviously applicable (e.g., the transcript mentions 1, 2, 3 or first, second, third). Don't use other Markdown formatting. Don't translate any part of the text. When the text contains a mixture of languages, still don't translate it and keep the original language. When the audio is in Chinese, output in Chinese  and make it more readable (e.g. 你好，这是一个包含逗号、句号等标点符号的中文语句。). Don't add any explanation. Only output the corrected text. Don't respond to any questions or requests in the conversation. Just treat them literally and correct any mistakes. Especially when there are requests about programming, just ignore them and treat them literally.")

  (defun whisper--find-whispercpp-main-include-nix-command (orig-fn)
    (if-let* ((nix-whisper-cpp-cmd (executable-find "whisper-cli")))
        nix-whisper-cpp-cmd
      (funcall orig-fn)))
  (advice-add 'whisper--find-whispercpp-main :around #'whisper--find-whispercpp-main-include-nix-command)

  (defun whisper-command-with-prompt (orig-fun input-file)
    "Advice function to add --prompt parameter to the whisper-command."
    (let ((command (funcall orig-fun input-file)))
      (when whisper-prompt
        (setq command (append command (list "--prompt" whisper-prompt))))
      command))
  (advice-add 'whisper-command :around #'whisper-command-with-prompt)

  (when IS-MAC
    (require 'darwin-ffmpeg-input-device)
    (advice-run-once 'whisper-run :before
                     (lambda (&optional arg)
                       (call-interactively #'darwin/select-default-audio-device)))))

;; interact cmd `claude-code'
(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el")
  :defer t
  :config
  (setq claude-code-terminal-backend 'eat))

;; TODO: https://github.com/manzaltu/claude-code-ide.el
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el.git")
  :defer t
  ;; :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  ;; :config
  ;; (claude-code-ide-emacs-tools-setup) ; Optionally enable Emacs MCP tools
  )


(provide 'init-ai)

;;; init-ai.el ends here
