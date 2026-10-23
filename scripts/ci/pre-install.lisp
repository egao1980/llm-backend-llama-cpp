;;;; Stream-extract OCI layers. chipz:DECOMPRESS (buffer API) holds the gzip
;;;; blob and doubles the output vector. llama-cpp linux/amd64
;;;; native-library.tar.gz is 1.7GB gzip / ~2.0GB tar (largest member
;;;; libcublasLt.so.12, ~718MB). That doubling exhausts the 4GB SBCL heap
;;;; (dynamic_space_size = 4294967296). Stream gunzip → tar so peak is the
;;;; blob plus one entry. SBCL has Gray streams; this CI is SBCL-only.

(in-package :cl-repository-client/installer)

(defun %stream-extract-tar-gz (tar-gz-data target-dir &key strip-prefix)
  (let* ((bytes (if (typep tar-gz-data '(simple-array (unsigned-byte 8) (*)))
                    tar-gz-data
                    (coerce tar-gz-data '(simple-array (unsigned-byte 8) (*)))))
         (input (flexi-streams:make-in-memory-input-stream bytes))
         (gz (chipz:make-decompressing-stream 'chipz:gzip input)))
    (extract-tar-stream gz target-dir :strip-prefix strip-prefix)))

(defun extract-layer-stripping-prefix (tar-gz-data target-dir prefix)
  (%stream-extract-tar-gz tar-gz-data target-dir :strip-prefix prefix))

(defun extract-layer (tar-gz-data target-dir)
  (%stream-extract-tar-gz tar-gz-data target-dir))

(format t "~&; ci: stream-extract OCI layers (chipz Gray stream)~%")
