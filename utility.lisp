(in-package :tales)

(defun asdf-base-path (name)
  (directory-namestring (asdf:component-pathname (asdf:find-system name))))

(defun tales-file (base)
  (concatenate 'string (asdf-base-path :tales) base))

(defun slurp-file (filename &optional external-format)
  (with-input-from-file (stream filename :external-format (or external-format :utf-8))
    (let ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      seq)))

(defun slurp-stream (stream)
  "Slurp all octets from STREAM. Returns a vector of octets."
  (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8))))
    (read-sequence seq stream)
    seq))

(defun slurp-as-octets (filename)
  "Slurp the contents of the file designated by FILENAME, returning
a vector of octets."
  (with-input-from-file (f filename :element-type '(unsigned-byte 8))
    (slurp-stream f)))

(defun last1 (list)
  (car (last list)))

(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
  (if (listp list)
      list
      (list list)))

