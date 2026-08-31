(in-package #:llm-backend-llama-cpp)

;;; JSON Schema → GBNF (llama.cpp dialect). Subset used by schema-protocol-json:
;;; object/array/string/number/integer/boolean/null, properties, required,
;;; additionalProperties (false default), items, min/maxItems, min/maxLength,
;;; enum, const, anyOf/oneOf, type unions, $ref into $defs / definitions.

(defparameter +gbnf-space+ "| \" \" | \"\\n\"{1,2} [ \\t]{0,20}")

(defparameter +gbnf-primitives+
  '(("boolean" . ("(\"true\" | \"false\")" . ()))
    ("decimal-part" . ("[0-9]{1,16}" . ()))
    ("integral-part" . ("[0] | [1-9] [0-9]{0,15}" . ()))
    ("number" . ("(\"-\"? integral-part) (\".\" decimal-part)? ([eE] [-+]? integral-part)?"
                 . ("integral-part" "decimal-part")))
    ("integer" . ("(\"-\"? integral-part)" . ("integral-part")))
    ("char" . ("[^\"\\\\\\x7F\\x00-\\x1F] | [\\\\] ([\"\\\\bfnrt] | \"u\" [0-9a-fA-F]{4})" . ()))
    ("string" . ("\"\\\"\" char* \"\\\"\"" . ("char")))
    ("null" . ("\"null\"" . ()))
    ("value" . ("object | array | string | number | boolean | null"
                . ("object" "array" "string" "number" "boolean" "null")))
    ("object" . ("\"{\" space ( string \":\" space value (\",\" space string \":\" space value)* )? space \"}\""
                 . ("string" "value")))
    ("array" . ("\"[\" space ( value (\",\" space value)* )? space \"]\"" . ("value")))))

(defun %ht (x)
  (and (hash-table-p x) x))

(defun %ht-get (table key)
  (when (hash-table-p table)
    (or (gethash key table)
        (and (stringp key) (gethash (intern (string-upcase key) :keyword) table)))))

(defun %as-list (x)
  (cond
    ((null x) '())
    ((stringp x) (list x))
    ((or (listp x) (vectorp x)) (coerce x 'list))
    (t (list x))))

(defun %stringish (x)
  (cond
    ((stringp x) x)
    ((keywordp x) (string-downcase (symbol-name x)))
    ((symbolp x) (string-downcase (symbol-name x)))
    (t (princ-to-string x))))

(defun %rule-name (name)
  (let* ((raw (%stringish (or name "root")))
         (out (with-output-to-string (o)
                (loop for c across raw
                      do (write-char (if (or (alphanumericp c) (char= c #\-))
                                         (char-downcase c)
                                         #\-)
                                     o)))))
    (if (zerop (length out)) "root" out)))

(defun %gbnf-escape (s)
  (with-output-to-string (o)
    (loop for c across s
          do (case c
               (#\" (write-string "\\\"" o))
               (#\\ (write-string "\\\\" o))
               (#\Newline (write-string "\\n" o))
               (#\Return (write-string "\\r" o))
               (t (write-char c o))))))

(defun %gbnf-literal (s)
  (format nil "\"~a\"" (%gbnf-escape s)))

(defun %json-encode (value)
  (cond
    ((eq value :null) "null")
    ((eq value :false) "false")
    ((eq value :true) "true")
    ((eq value t) "true")
    ((null value) "null")
    ((stringp value)
     (with-output-to-string (o)
       (write-char #\" o)
       (loop for c across value
             do (case c
                  (#\" (write-string "\\\"" o))
                  (#\\ (write-string "\\\\" o))
                  (#\Newline (write-string "\\n" o))
                  (#\Return (write-string "\\r" o))
                  (#\Tab (write-string "\\t" o))
                  (t (write-char c o))))
       (write-char #\" o)))
    ((integerp value) (princ-to-string value))
    ((realp value) (princ-to-string (float value 1d0)))
    ((symbolp value) (%json-encode (string-downcase (symbol-name value))))
    (t (error 'llm-error
              :message (format nil "cannot JSON-encode ~s for GBNF" value)))))

(defun %sorted-keys (ht)
  (let ((keys '()))
    (maphash (lambda (k v) (declare (ignore v)) (push (%stringish k) keys)) ht)
    (sort keys #'string<)))

(defstruct (%gbnf-cx (:constructor %make-gbnf-cx (root)))
  root
  (rules (make-hash-table :test #'equal))
  (order '())
  (refs-seen (make-hash-table :test #'equal)))

(defun %add-rule (cx name body)
  (let ((name (%rule-name name)))
    (loop for n = name then (format nil "~a-~a" name i)
          for i from 2
          for existing = (gethash n (%gbnf-cx-rules cx))
          do (cond
               ((null existing)
                (setf (gethash n (%gbnf-cx-rules cx)) body)
                (push n (%gbnf-cx-order cx))
                (return n))
               ((string= existing body)
                (return n))))))

(defun %add-primitive-as (cx name prim)
  (let ((spec (assoc prim +gbnf-primitives+ :test #'string=)))
    (unless spec
      (error 'llm-error :message (format nil "unknown GBNF primitive ~s" prim)))
    (destructuring-bind (body . deps) (cdr spec)
      (let ((n (%add-rule cx name body)))
        (dolist (d deps)
          (unless (gethash d (%gbnf-cx-rules cx))
            (%add-primitive-as cx d d)))
        n))))

(defun %add-primitive (cx name)
  (%add-primitive-as cx name name))

(defun %resolve-pointer (root pointer)
  (let* ((p (or pointer ""))
         (path (if (and (plusp (length p)) (char= (char p 0) #\#))
                   (subseq p 1)
                   p)))
    (when (and (plusp (length path)) (char= (char path 0) #\/))
      (setf path (subseq path 1)))
    (if (zerop (length path))
        root
        (let ((cur root))
          (dolist (seg (uiop:split-string path :separator "/"))
            (setf cur (cond
                        ((not (%ht cur)) nil)
                        ((%ht-get cur seg))
                        (t nil))))
          cur))))

(defun %schema-type (schema)
  (let ((ty (%ht-get schema "type")))
    (cond
      ((stringp ty) (list ty))
      ((or (listp ty) (vectorp ty)) (mapcar #'%stringish (%as-list ty)))
      (t nil))))

(defun %visit-union (cx name alts)
  (let ((parts (loop for alt in (%as-list alts)
                     for i from 0
                     collect (%visit cx alt (format nil "~a-alt-~a" (or name "root") i)))))
    (%add-rule cx (or name "root") (format nil "~{~a~^ | ~}" parts))))

(defun %as-count (n)
  (cond
    ((null n) nil)
    ((integerp n) n)
    ((realp n) (truncate n))
    (t nil)))

(defun %repetition (item min max separator)
  (let ((min (or (%as-count min) 0))
        (max (%as-count max))
        (tail (and separator (format nil "(~a ~a)" separator item))))
    (cond
      ((and max (zerop max)) "")
      ((null separator)
       (cond
         ((and (zerop min) max (= max 1)) (format nil "~a?" item))
         ((and (= min 1) (null max)) (format nil "~a+" item))
         ((and (zerop min) (null max)) (format nil "~a*" item))
         (t (format nil "~a{~a,~@[~a~]}" item min max))))
      ((and (= min 1) (null max)) (format nil "~a ~a*" item tail))
      ((and (zerop min) (null max)) (format nil "(~a ~a*)?" item tail))
      ((and (zerop min) max (= max 1)) (format nil "~a?" item))
      ((and max (= min max))
       (if (= min 1)
           item
           (format nil "~a ~a{~a}" item tail (1- min))))
      (t (format nil "~a{~a,~@[~a~]}" item min max)))))

(defun %visit-array (cx schema name)
  (let* ((items (%ht-get schema "items"))
         (prefix (%ht-get schema "prefixItems"))
         (min (%ht-get schema "minItems"))
         (max (%ht-get schema "maxItems")))
    (cond
      ((and prefix (or (consp prefix)
                       (and (vectorp prefix) (not (stringp prefix)))))
       (let ((parts (loop for item in (%as-list prefix)
                          for i from 0
                          collect (%visit cx item (format nil "~a-tuple-~a" name i)))))
         (%add-rule cx name
                    (format nil "\"[\" space ~{~a~^ \",\" space ~} space \"]\"" parts))))
      (t
       (let ((item-rule (%visit cx (or items (make-hash-table :test #'equal))
                                (format nil "~a-item" name))))
         (%add-rule cx name
                    (format nil "\"[\" space ~a space \"]\""
                            (%repetition item-rule min max "\",\" space"))))))))

(defun %visit-object (cx schema name)
    (let* ((props (%ht (%ht-get schema "properties")))
         (required (mapcar #'%stringish (%as-list (%ht-get schema "required"))))
         (additional (%ht-get schema "additionalProperties"))
         (prop-names (if props (%sorted-keys props) '()))
         (kv (make-hash-table :test #'equal))
         (req '())
         (opt '()))
    (dolist (pname (append required
                           (remove-if (lambda (k) (member k required :test #'string=))
                                      prop-names)))
      (unless (gethash pname kv)
        (let* ((pschema (and props (or (%ht-get props pname)
                                       (gethash pname props))))
               (prule (%visit cx (or pschema (make-hash-table :test #'equal))
                              (format nil "~a-~a" name pname)))
               (kvr (%add-rule cx (format nil "~a-~a-kv" name pname)
                               (format nil "~a space \":\" space ~a"
                                       (%gbnf-literal (%json-encode pname))
                                       prule))))
          (setf (gethash pname kv) kvr)
          (if (member pname required :test #'string=)
              (push pname req)
              (push pname opt)))))
    (setf req (nreverse req) opt (nreverse opt))
    (when (or (eq additional t) (%ht additional))
      (let* ((val (if (%ht additional)
                      (%visit cx additional (format nil "~a-additional-value" name))
                      (%add-primitive cx "value")))
             (key (if prop-names
                      (%add-rule cx (format nil "~a-additional-k" name)
                                 "string")
                      (%add-primitive cx "string")))
             (akv (%add-rule cx (format nil "~a-additional-kv" name)
                             (format nil "~a \":\" space ~a" key val))))
        (setf (gethash "*" kv) akv)
        (push "*" opt)))
    (let ((rule (with-output-to-string (o)
                  (write-string "\"{\" space " o)
                  (loop for i from 0 for k in req
                        do (when (plusp i) (write-string " \",\" space " o))
                           (write-string (gethash k kv) o))
                  (when opt
                    (write-string " (" o)
                    (when req
                      (write-string " \",\" space ( " o))
                    (labels ((rest-refs (ks first-optional)
                               (if (null ks)
                                   ""
                                   (let* ((k (first ks))
                                          (kv-name (gethash k kv))
                                          (comma (format nil "( \",\" space ~a )" kv-name))
                                          (here (if first-optional
                                                    (format nil "~a~a" comma
                                                            (if (string= k "*") "*" "?"))
                                                    (if (string= k "*")
                                                        (format nil "~a ~a*" kv-name comma)
                                                        kv-name))))
                                     (if (rest ks)
                                         (format nil "~a ~a" here
                                                 (%add-rule cx (format nil "~a-~a-rest" name k)
                                                            (rest-refs (rest ks) t)))
                                         here)))))
                      (loop for i from 0
                            for tail on opt
                            do (when (plusp i) (write-string " | " o))
                               (write-string (rest-refs tail nil) o)))
                    (when req (write-string " )" o))
                    (write-string " )?" o))
                  (write-string " space \"}\"" o))))
      (%add-rule cx name rule))))

(defun %visit (cx schema name)
  (let ((schema (or (%ht schema) (make-hash-table :test #'equal)))
        (name (or name "root")))
    (cond
      ((%ht-get schema "$ref")
       (let* ((ref (%stringish (%ht-get schema "$ref")))
              (target (%resolve-pointer (%gbnf-cx-root cx) ref)))
         (unless target
           (error 'llm-error :message (format nil "unresolved $ref ~s" ref)))
         (if (gethash ref (%gbnf-cx-refs-seen cx))
             (%add-rule cx (format nil "ref~a" (%rule-name ref)) "value")
             (progn
               (setf (gethash ref (%gbnf-cx-refs-seen cx)) t)
               (%visit cx target (format nil "ref~a" (%rule-name ref)))))))
      ((or (%ht-get schema "oneOf") (%ht-get schema "anyOf"))
       (%visit-union cx name (or (%ht-get schema "oneOf") (%ht-get schema "anyOf"))))
      ((let ((ty (%ht-get schema "type")))
         (and ty (or (listp ty) (and (vectorp ty) (not (stringp ty))))))
       (%visit-union cx name
                     (mapcar (lambda (tname)
                               (let ((h (make-hash-table :test #'equal)))
                                 (maphash (lambda (k v) (setf (gethash k h) v)) schema)
                                 (setf (gethash "type" h) tname)
                                 h))
                             (%as-list (%ht-get schema "type")))))
      ((nth-value 1 (gethash "const" schema))
       (%add-rule cx name (%gbnf-literal (%json-encode (gethash "const" schema)))))
      ((%ht-get schema "enum")
       (%add-rule cx name
                  (format nil "(~{~a~^ | ~})"
                          (mapcar (lambda (v) (%gbnf-literal (%json-encode v)))
                                  (%as-list (%ht-get schema "enum"))))))
      ((or (member "object" (%schema-type schema) :test #'string=)
           (%ht-get schema "properties")
           (nth-value 1 (gethash "additionalProperties" schema)))
       (if (or (%ht-get schema "properties")
               (nth-value 1 (gethash "additionalProperties" schema)))
           (%visit-object cx schema name)
           (%add-primitive-as cx name "object")))
      ((or (member "array" (%schema-type schema) :test #'string=)
           (%ht-get schema "items")
           (%ht-get schema "prefixItems"))
       (%visit-array cx schema name))
      ((and (member "string" (%schema-type schema) :test #'string=)
            (or (%ht-get schema "minLength") (%ht-get schema "maxLength")))
       (let ((char (%add-primitive cx "char"))
             (min (%ht-get schema "minLength"))
             (max (%ht-get schema "maxLength")))
         (%add-rule cx name
                    (format nil "\"\\\"\" ~a \"\\\"\""
                            (%repetition char (or min 0) max nil)))))
      ((let ((ty (%schema-type schema)))
         (and (= (length ty) 1)
              (assoc (first ty) +gbnf-primitives+ :test #'string=)))
       (%add-primitive-as cx name (first (%schema-type schema))))
      ((or (zerop (hash-table-count schema))
           (and (null (%schema-type schema))
                (null (%ht-get schema "properties"))))
       (%add-primitive-as cx name "value"))
      (t
       (error 'llm-error
              :message (format nil "unsupported JSON Schema for GBNF: ~s"
                               (loop for k being the hash-keys of schema collect k)))))))

(defun json-schema-to-gbnf (schema)
  "Compile a JSON Schema hash-table to a GBNF string (root rule)."
  (let ((schema (or (%ht schema)
                    (error 'llm-error :message "json-schema-to-gbnf needs a hash-table"))))
    (let* ((cx (%make-gbnf-cx schema)))
      (setf (gethash "space" (%gbnf-cx-rules cx)) +gbnf-space+)
      (push "space" (%gbnf-cx-order cx))
      (%visit cx schema "root")
      (with-output-to-string (o)
        (dolist (name (reverse (%gbnf-cx-order cx)))
          (format o "~a ::= ~a~%" name (gethash name (%gbnf-cx-rules cx))))))))

(defun llama-cpp-settings (&key temperature max-tokens stop top-p response-format
                            output extra grammar grammar-root)
  "LLM-SETTINGS with GBNF stashed in EXTRA. Do not subclass LLM-SETTINGS —
   COPY-LLM-SETTINGS rebuilds a plain instance."
  (make-llm-settings
   :temperature temperature :max-tokens max-tokens :stop stop :top-p top-p
   :response-format response-format :output output
   :extra (append (cond
                    ((null extra) nil)
                    ((consp extra) (copy-list extra))
                    (t (error 'llm-error
                              :message "llama-cpp-settings :extra must be a plist")))
                  (and grammar (list :grammar grammar))
                  (and grammar-root (list :grammar-root grammar-root)))))
