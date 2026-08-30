(in-package #:llm-backend-llama-cpp)

(defvar *complete-fn* #'llama-cpp:complete
  "Injected for tests. (lambda (engine prompt &key max-tokens temperature)
    → (values text prompt-tokens completion-tokens)).")

(defvar *embed-fn* #'llama-cpp:embed
  "Injected for tests. (lambda (engine texts) → (values vectors dim prompt-tokens)).")

(defun %env (name)
  (let ((v (uiop:getenv name)))
    (and v (plusp (length v)) v)))

(defclass llama-cpp-backend (llm-backend)
  ((model-path :initarg :model-path :accessor llama-cpp-model-path :initform nil)
   (n-ctx :initarg :n-ctx :accessor llama-cpp-n-ctx :initform 2048)
   (engine :initarg :engine :accessor llama-cpp-engine :initform nil)))

(defun make-llama-cpp-backend (&key model-path n-ctx engine)
  (make-instance 'llama-cpp-backend
                 :model-path (or model-path (%env "LLAMA_MODEL_PATH")
                                 (%env "LLAMA_CPP_MODEL"))
                 :n-ctx (or n-ctx 2048)
                 :engine engine))

(defun use-llama-cpp-backend (&rest args &key &allow-other-keys)
  (setf *llm-backend* (apply #'make-llama-cpp-backend args)))

(defun close-llama-cpp-backend (backend)
  (when (and (llama-cpp-engine backend)
             (llama-cpp:llama-engine-p (llama-cpp-engine backend)))
    (llama-cpp:free-engine (llama-cpp-engine backend))
    (setf (llama-cpp-engine backend) nil))
  backend)

(defun ensure-llama-cpp-engine (backend)
  (or (llama-cpp-engine backend)
      (setf (llama-cpp-engine backend)
            (llama-cpp:load-engine :model-path (llama-cpp-model-path backend)
                                   :n-ctx (llama-cpp-n-ctx backend)))))

(defmethod backend-model ((backend llama-cpp-backend))
  (or (llama-cpp-model-path backend)
      (and (llama-cpp-engine backend)
           (llama-cpp:llama-engine-p (llama-cpp-engine backend))
           (llama-cpp:engine-model-path (llama-cpp-engine backend)))))

(defmethod backend-supports-p ((backend llama-cpp-backend) (feature (eql :embeddings)))
  t)

(defmethod backend-supports-p ((backend llama-cpp-backend) (feature (eql :stream)))
  nil)

(defmethod backend-supports-p ((backend llama-cpp-backend) (feature (eql :tools)))
  nil)

(defmethod backend-supports-p ((backend llama-cpp-backend) (feature (eql :responses)))
  nil)

(defun %prompt (turns)
  (let ((ts (coerce-turns turns)))
    (or (loop for turn in (reverse ts)
              when (eq (llm-turn-role turn) :user)
                return (turn-text turn))
        (and ts (turn-text (car (last ts))))
        "")))

(defmethod generate ((backend llama-cpp-backend) turns &key model settings tools
                     tool-choice output)
  (declare (ignore tools tool-choice output))
  (let* ((settings (coerce-settings settings))
         (engine (ensure-llama-cpp-engine backend)))
    (multiple-value-bind (text pt ct)
        (funcall *complete-fn* engine (%prompt turns)
                 :max-tokens (or (and settings (llm-settings-max-tokens settings)) 32)
                 :temperature (or (and settings (llm-settings-temperature settings)) 0.0))
      (make-llm-response
       :parts (list (make-llm-text-part :text (or text "")))
       :model (or model (backend-model backend))
       :finish-reason :stop
       :usage (make-llm-usage :input-tokens pt :output-tokens ct
                              :total-tokens (and pt ct (+ pt ct)))))))

(defmethod list-models ((backend llama-cpp-backend) &key)
  (list (make-llm-model-info
         :id (or (backend-model backend) "llama.cpp")
         :owned-by "llama.cpp")))

(defun %slice-embedding (vec dimensions)
  (cond
    ((null dimensions) vec)
    ((> dimensions (length vec))
     (error 'llm-error
            :message (format nil "requested dimensions ~a > model dim ~a"
                             dimensions (length vec))))
    (t (subseq vec 0 dimensions))))

(defmethod embed ((backend llama-cpp-backend) inputs &key model dimensions
                  encoding-format)
  (when (and encoding-format
             (not (member encoding-format '(:float "float") :test #'equal)))
    (error 'llm-unsupported
           :message (format nil "llama.cpp embeddings are float-only, got ~s"
                            encoding-format)))
  (let* ((texts (coerce-embed-inputs inputs))
         (engine (ensure-llama-cpp-engine backend)))
    (multiple-value-bind (vecs dim tokens)
        (funcall *embed-fn* engine texts)
      (declare (ignore dim))
      (make-llm-embed-result
       :embeddings (loop for v in vecs for i from 0
                         collect (make-llm-embedding
                                  :vector (%slice-embedding v dimensions)
                                  :index i))
       :model (or model (backend-model backend))
       :usage (make-llm-usage :input-tokens tokens :total-tokens tokens)))))
