(in-package #:qoi)

(defvar *buffer*)

(defmacro with-buffer-read ((&key octets stream) &body body)
  `(let ((*buffer* (io:make-input-buffer :vector ,octets :stream ,stream)))
     ,@body))

(declaim (inline read-octets))
(defun read-octets (count)
  (declare (optimize speed))
  (let ((octets (io:make-octet-vector count)))
    (io:fast-read-sequence octets *buffer*)
    octets))

(declaim (inline read-ub8))
(defun read-ub8 ()
  (declare (optimize speed))
  (io:fast-read-byte *buffer*))

(declaim (inline read-ub24))
(defun read-ub24 ()
  (declare (optimize speed))
  (let ((value 0))
    (setf (ldb (byte 8 16) value) (read-ub8)
          (ldb (byte 8 8) value) (read-ub8)
          (ldb (byte 8 0) value) (read-ub8))
    value))

(declaim (inline read-ub32))
(defun read-ub32 ()
  (declare (optimize speed))
  (let ((value 0))
    (setf (ldb (byte 8 24) value) (read-ub8)
          (ldb (byte 8 16) value) (read-ub8)
          (ldb (byte 8 8) value) (read-ub8)
          (ldb (byte 8 0) value) (read-ub8))
    value))

(declaim (inline read-ascii-string))
(defun read-ascii-string (octet-count)
  (declare (optimize speed))
  (let ((octets (read-octets octet-count)))
    (babel:octets-to-string octets :encoding :ascii)))
