(defpackage #:llm-backend-llama-cpp
  (:use #:cl #:llm-protocol)
  (:nicknames #:stack-llm-llama-cpp)
  (:export #:llama-cpp-backend
           #:make-llama-cpp-backend
           #:use-llama-cpp-backend
           #:llama-cpp-model-path
           #:llama-cpp-engine
           #:llama-cpp-chat-template
           #:ensure-llama-cpp-engine
           #:close-llama-cpp-backend
           #:llama-cpp-settings
           #:apply-chat-template
           #:infer-chat-template
           #:json-schema-to-gbnf
           #:*complete-fn*
           #:*embed-fn*))

(in-package #:llm-backend-llama-cpp)
