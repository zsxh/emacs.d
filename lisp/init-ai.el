;; init-ai.el --- AI packages	-*- lexical-binding: t -*-

;; Author: Zsxh Chen <bnbvbchen@gmail.com>
;; URL: https://github.com/zsxh/emacs.d

;;; Commentary:
;;
;;  AI packages
;;

;;; Code:

;; A simple ChatGPT client for Emacs
;; https://github.com/karthink/gptel
;; NOTE: https://github.com/karthink/gptel/wiki#defining-custom-gptel-commands
;; NOTE: https://ollama.com/blog/how-to-prompt-code-llama
(use-package gptel
  :defer t
  :bind (:map gptel-mode-map
         ("C-c h" . gptel-menu))
  :config
  (setq gptel-proxy (format "http://%s:%s" personal-proxy-http-host personal-proxy-http-port)
        gptel-default-mode 'org-mode
        gptel-expert-commands t
        gptel-log-level 'debug)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (add-hook 'gptel-mode-hook 'turn-on-visual-line-mode)
  ;; Clean Up default backends
  (setq gptel--known-backends nil)
  ;; OpenRouter
  (when (bound-and-true-p personal-openrouter-key)
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key 'personal-openrouter-key
      :models '(anthropic/claude-3-5-haiku
                anthropic/claude-3.5-sonnet
                google/gemini-2.0-flash-exp:free
                google/gemini-exp-1206:free)))
  ;; DeepSeek
  (when (bound-and-true-p personal-deepseek-key)
    ;; default backend
    (setq gptel-backend (gptel-make-openai "DeepSeek"
                          :key 'personal-deepseek-key
                          :stream t
                          :models '(deepseek-chat)
                          :host "api.deepseek.com"))
    ;; defualt model
    (setq gptel-model 'deepseek-chat)))

(use-package gptel-commit
  :ensure nil
  :commands (gptel-commit))

;; https://github.com/s-kostyaev/ellama
(use-package llm
  :defer t)

(use-package ellama
  :commands (ellama-transient-main-menu)
  :config
  (setq ellama-session-auto-save t
        ellama-sessions-directory (file-truename
                                   (file-name-concat
                                    user-emacs-directory
                                    "cache"
                                    "ellama-sessions")))
  (require 'llm-openai)
  (setopt ellama-providers
          '(("deepseek" . (make-llm-openai-compatible
                           :url "https://api.deepseek.com/v1/"
                           :chat-model "deepseek-chat"
                           :key personal-deepseek-key))
            ("claude-3.5-haiku" . (make-llm-openai-compatible
                                   :url "https://openrouter.ai/api/v1/"
                                   :chat-model "anthropic/claude-3-5-haiku"
                                   :key personal-openrouter-key))
            ("claude-3.5-sonnet" . (make-llm-openai-compatible
                                    :url "https://openrouter.ai/api/v1/"
                                    :chat-model "anthropic/claude-3.5-sonnet"
                                    :key personal-openrouter-key))))
  (setopt ellama-provider (eval (cdar ellama-providers))))

;; TODO: RAG [elisa](https://github.com/s-kostyaev/elisa)
;; TODO: https://github.com/zbelial/eureka.el


(provide 'init-ai)

;;; init-ai.el ends here
