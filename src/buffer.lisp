(in-package #:cl-qoi)

(defvar *buffer*)

(defmacro with-buffer-read ((&key octets stream) &body body)
  `(let ((*buffer* (io:make-input-buffer :vector ,octets :stream ,stream)))
     ,@body))

(defun read-octets (count)
  (let ((octets (io:make-octet-vector count)))
    (io:fast-read-sequence octets *buffer*)
    octets))

(defun read-uint (octet-count)
  (loop :with value = 0
        :for i :from (* (1- octet-count) 8) :downto 0 :by 8
        :for byte = (io:fast-read-byte *buffer*)
        :do (setf (ldb (byte 8 i) value) byte)
        :finally (return value)))

(defun read-ascii-string (octet-count)
  (let ((octets (read-octets octet-count)))
    (babel:octets-to-string octets :encoding :ascii)))
