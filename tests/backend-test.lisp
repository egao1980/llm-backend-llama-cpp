(in-package #:llm-backend-llama-cpp/tests)

(defun %fake-complete (engine prompt &key max-tokens temperature)
  (declare (ignore engine max-tokens temperature))
  (values (format nil "ok:~a" prompt) 3 2))

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
  (let ((cat (make-llm-catalogue (%backend))))
    (ok (capability-protocol:capability-supported-p cat :llm-embeddings))
    (ok (not (capability-protocol:capability-supported-p cat :llm-tools)))))
