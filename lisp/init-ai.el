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
        gptel-expert-commands t
        gptel-log-level 'debug)
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
                anthropic/claude-3.5-sonnet)))
  ;; DeepSeek
  (when (bound-and-true-p personal-deepseek-key)
    ;; default backend
    (setq gptel-backend (gptel-make-openai "DeepSeek"
                          :key 'personal-deepseek-key
                          :stream t
                          :models '(deepseek-chat
                                    deepseek-reasoner)
                          :host "api.deepseek.com"))
    ;; defualt model
    (setq gptel-model 'deepseek-chat)))

(use-package gptel-commit
  :ensure nil
  :commands (gptel-commit))

;; TODO: ai tools
;; - RAG: https://github.com/s-kostyaev/elisa
;; - ai assistant :https://github.com/zbelial/eureka.el
;; - code fim: https://github.com/milanglacier/minuet-ai.el


(provide 'init-ai)

;;; init-ai.el ends here
