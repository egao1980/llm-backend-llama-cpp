(defsystem "llm-backend-llama-cpp"
  :version "0.1.0"
  :description "llm-protocol backend over llama-cpp (ggml-org/llama.cpp)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("llm-protocol" "llama-cpp")
  :properties
  (:cl-repo
   (:ci (:with ("capability-protocol"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "backend"))
  :in-order-to ((test-op (test-op "llm-backend-llama-cpp/tests"))))

(defsystem "llm-backend-llama-cpp/tests"
  :depends-on ("llm-backend-llama-cpp" "llm-protocol/capability" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
