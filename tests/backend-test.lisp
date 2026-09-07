(in-package #:llm-backend-llama-cpp/tests)

(defun %fake-complete (engine prompt &key max-tokens temperature grammar grammar-root
                       on-token)
  (declare (ignore engine max-tokens temperature))
  (let ((text (if (search "Available tools" prompt)
                  "{\"name\":\"sum\",\"arguments\":{\"a\":1}}"
                  (format nil "ok:~a~@[:~a~]~@[:~a~]" prompt grammar grammar-root))))
    (when on-token
      (loop for i from 0 below (length text)
            until (funcall on-token (string (char text i)))))
    (values text 3 2)))

(defun %fake-embed (engine texts)
  (declare (ignore engine))
  (values (loop for s in texts
                collect (let ((v (make-array 3 :element-type 'single-float
                                             :initial-element 0f0)))
                          (setf (aref v 0) (float (length s) 1f0))
                          v))
          3
          (reduce #'+ texts :key #'length)))

(defmacro %with-fake (&body body)
  `(let ((llm-backend-llama-cpp:*complete-fn* #'%fake-complete)
         (llm-backend-llama-cpp:*embed-fn* #'%fake-embed))
     ,@body))

(defun %backend ()
  (llm-backend-llama-cpp:make-llama-cpp-backend
   :model-path "/models/fake"
   :engine :fake))

(deftest generate-mock
  (%with-fake
    (let* ((b (%backend))
           (r (generate b "hi" :model "local")))
      (ok (equal "ok:hi" (llm-response-text r)))
      (ok (equal "local" (llm-response-model r)))
      (ok (eq :stop (llm-response-finish-reason r)))
      (ok (= 5 (llm-usage-total-tokens (llm-response-usage r)))))))

(deftest list-models
  (let ((models (list-models (%backend))))
    (ok (equal "/models/fake" (llm-model-info-id (first models))))
    (ok (equal "llama.cpp" (llm-model-info-owned-by (first models))))))

(deftest embed-mock
  (%with-fake
    (let* ((b (%backend))
           (r (embed b "hi")))
      (ok (llm-embed-result-p r))
      (ok (equal "/models/fake" (llm-embed-result-model r)))
      (let ((v (llm-embedding-vector (first (llm-embed-result-embeddings r)))))
        (ok (= 3 (length v)))
        (ok (= 2f0 (aref v 0))))
      (ok (= 2 (llm-usage-total-tokens (llm-embed-result-usage r)))))))

(deftest embed-many-and-dimensions
  (%with-fake
    (let* ((r (embed (%backend) '("aa" "bbb") :dimensions 1))
           (embs (llm-embed-result-embeddings r)))
      (ok (= 2 (length embs)))
      (ok (= 1 (length (llm-embedding-vector (first embs)))))
      (ok (= 2f0 (aref (llm-embedding-vector (first embs)) 0)))
      (ok (= 3f0 (aref (llm-embedding-vector (second embs)) 0))))))

(deftest embed-rejects-base64
  (%with-fake
    (ok (signals (embed (%backend) "hi" :encoding-format :base64)
                 'llm-unsupported))))

(deftest catalogue
  (let* ((cat (make-llm-catalogue (%backend)))
         (gen (capability-protocol:get-capability cat :llm-generation)))
    (ok (capability-protocol:capability-supported-p cat :llm-embeddings))
    (ok (capability-protocol:capability-supported-p cat :llm-structured-output))
    (ok (capability-protocol:capability-supported-p cat :llm-tools)))
    (ok (find 'capability-protocol:stream-complete
              (capability-protocol:capability-operations gen)
              :key #'capability-protocol:capability-operation-name))))

(deftest supports-grammar
  (let ((b (%backend)))
    (ok (backend-supports-p b :grammar))
    (ok (backend-supports-p b :structured-output))
    (ok (backend-supports-p b :stream))
    (ok (backend-supports-p b :tools))))

(deftest stream-generate-mock
  (%with-fake
    (let* ((seen '())
           (r (stream-generate (%backend) "hi" :model "local"
                               :on-part (lambda (p)
                                          (push (llm-text-part-text p) seen)))))
      (ok (equal "ok:hi" (llm-response-text r)))
      (ok (equal "ok:hi" (apply #'concatenate 'string (reverse seen))))
      (ok (equal "local" (llm-response-model r)))
      (ok (eq :stop (llm-response-finish-reason r))))))

(deftest stream-generate-raw-grammar
  (%with-fake
    (let* ((seen '())
           (r (stream-generate
               (%backend) "hi"
               :settings (llm-backend-llama-cpp:llama-cpp-settings
                          :grammar "root ::= \"x\""
                          :grammar-root "root")
               :on-part (lambda (p)
                          (push (llm-text-part-text p) seen)))))
      (ok (equal "ok:hi:root ::= \"x\":root" (llm-response-text r)))
      (ok (equal "ok:hi:root ::= \"x\":root"
                 (apply #'concatenate 'string (reverse seen)))))))

(deftest generate-raw-grammar
  (%with-fake
    (let* ((b (%backend))
           (r (generate b "hi"
                        :settings (llm-backend-llama-cpp:llama-cpp-settings
                                   :grammar "root ::= \"x\""
                                   :grammar-root "root"))))
      (ok (equal "ok:hi:root ::= \"x\":root" (llm-response-text r))))))

(deftest generate-output-schema-to-grammar
  (%with-fake
    (let* ((schema (%js "type" "object"
                        "properties" (%js "name" (%js "type" "string"))
                        "required" #("name")))
           (r (generate (%backend) "city" :output schema)))
      (ok (search "root ::=" (llm-response-text r)))
      (ok (search "name" (llm-response-text r))))))

(deftest extra-grammar-wins-over-output
  (%with-fake
    (let ((r (generate (%backend) "x"
                       :output (%js "type" "string")
                       :settings (llm-backend-llama-cpp:llama-cpp-settings
                                  :grammar "root ::= \"z\""))))
      (ok (equal "ok:x:root ::= \"z\":root" (llm-response-text r))))))

(deftest generate-tools-mock
  (%with-fake
    (let ((r (generate (%backend) "add"
                       :tools (list (make-llm-tool :name "sum")))))
      (ok (eq :tool-use (llm-response-finish-reason r)))
      (ok (equal "sum" (llm-tool-call-part-name
                        (first (llm-response-tool-calls r)))))
      (ok (search "\"a\":1" (llm-tool-call-part-arguments
                             (first (llm-response-tool-calls r))))))))

(deftest tools-inject-gbnf-and-prompt
  (let ((seen-prompt nil)
        (seen-grammar nil))
    (%with-fake
      (let ((llm-backend-llama-cpp:*complete-fn*
              (lambda (engine prompt &key grammar &allow-other-keys)
                (declare (ignore engine))
                (setf seen-prompt prompt
                      seen-grammar grammar)
                (values "{\"name\":\"sum\",\"arguments\":{}}" 1 1))))
        (generate (%backend) "add"
                  :tools (list (make-llm-tool :name "sum"
                                              :description "add numbers")))
        (ok (search "Available tools" seen-prompt))
        (ok (search "sum" seen-prompt))
        (ok (search "user: add" seen-prompt))
        (ok (search "name" seen-grammar))
        (ok (search "sum" seen-grammar))))))

(deftest extra-grammar-wins-over-tools
  (let ((seen-grammar nil))
    (%with-fake
      (let ((llm-backend-llama-cpp:*complete-fn*
              (lambda (engine prompt &key grammar &allow-other-keys)
                (declare (ignore engine prompt))
                (setf seen-grammar grammar)
                (values "z" 1 1))))
        (generate (%backend) "x"
                  :tools (list (make-llm-tool :name "sum"))
                  :settings (llm-backend-llama-cpp:llama-cpp-settings
                             :grammar "root ::= \"z\""))
        (ok (equal "root ::= \"z\"" seen-grammar))))))

(deftest stream-generate-tools
  (%with-fake
    (let ((r (stream-generate (%backend) "add"
                              :tools (list (make-llm-tool :name "sum")))))
      (ok (eq :tool-use (llm-response-finish-reason r)))
      (ok (equal "sum" (llm-tool-call-part-name
                        (first (llm-response-tool-calls r))))))))
