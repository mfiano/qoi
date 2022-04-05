(in-package #:cl-qoi)

(defvar *image*)

(u:define-constant +rgb-tag+ #b11111110)
(u:define-constant +rgba-tag+ #b11111111)
(u:define-constant +index-tag+ #b00)
(u:define-constant +diff-tag+ #b01)
(u:define-constant +luma-tag+ #b10)
(u:define-constant +run-tag+ #b11)

(defstruct (image
            (:constructor %make-image)
            (:conc-name nil))
  (width 0 :type u:ub32)
  (height 0 :type u:ub32)
  (channels :rgb :type (member :rgb :rgba))
  (color-space :srgb :type (member :srgb :linear))
  (data nil :type (u:ub8a (*))))

(u:define-printer (image stream :identity t)
  (format stream "~dx~d ~a/~a"
          (width image)
          (height image)
          (channels image)
          (color-space image)))

(defun make-image (width height channel-count color-space)
  (let ((channels (ecase channel-count
                    (3 :rgb)
                    (4 :rgba)))
        (color-space (ecase color-space
                       (0 :srgb)
                       (1 :linear))))
    (%make-image :width width
                 :height height
                 :channels channels
                 :color-space color-space
                 :data (u:make-ub8-array (* width height channel-count)))))

(defun read-header ()
  (with-buffer-read (:octets (read-octets 14))
    (unless (string= (read-ascii-string 4) "qoif")
      (error "Invalid QOI image."))
    (values (read-uint 4)
            (read-uint 4)
            (read-uint 1)
            (read-uint 1))))

(defun read-rgb ()
  (values (read-uint 3)
          1))

(defun read-rgba ()
  (values (read-uint 4)
          1))

(defun read-index (octet)
  (let ((pixel (aref *pixel-table* (ldb (byte 6 0) octet))))
    (values pixel 1)))

(defun read-diff (octet previous channel-count)
  (u:mvlet ((pixel 0)
            (r g b a (unpack-pixel previous channel-count))
            (db (ldb (byte 8 0) (- (ldb (byte 2 0) octet) 2)))
            (dg (ldb (byte 8 0) (- (ldb (byte 2 2) octet) 2)))
            (dr (ldb (byte 8 0) (- (ldb (byte 2 4) octet) 2))))
    (values (pack-pixel (- r dr) (- g dg) (- b db) a channel-count)
            1)))

(defun read-luma (octet previous channel-count)
  (u:mvlet ((pixel 0)
            (r g b a (unpack-pixel previous channel-count))
            (db-dg (ldb (byte 8 0) (- (ldb (byte 4 0) octet) 8)))
            (dr-dg (ldb (byte 8 0) (- (ldb (byte 4 4) octet) 8)))
            (dg (ldb (byte 8 0) (- (ldb (byte 6 8) octet) 32))))
    (values (pack-pixel (- r (+ dr-dg dg))
                        (- g dg)
                        (- b (+ db-dg dg))
                        a
                        channel-count)
            1)))

(defun read-run (octet previous)
  (let ((run (ldb (byte 8 0) (1+ (ldb (byte 6 0) octet)))))
    (values previous run)))

(defun read-chunk (previous channel-count)
  (let ((octet (read-uint 1)))
    (case octet
      (#.+rgb-tag+
       (read-rgb))
      (#.+rgba-tag+
       (read-rgba))
      (t
       (ecase (ldb (byte 2 6) octet)
         (#.+index-tag+ (read-index octet))
         (#.+diff-tag+ (read-diff octet previous channel-count))
         (#.+luma-tag+ (read-luma octet previous channel-count))
         (#.+run-tag+ (read-run octet previous)))))))

(defun read-data (image channel-count)
  (loop :with previous := #xff
        :with index := 0
        :with width := (width image)
        :with height := (height image)
        :with data := (data image)
        :while (< index (* width height channel-count))
        :do (u:mvlet* ((pixel run (read-chunk previous channel-count))
                       (r g b a (unpack-pixel pixel channel-count)))
              (add-seen-pixel r g b a channel-count)
              (dotimes (i run)
                (let ((offset (+ index i)))
                  (setf (aref data (+ offset 0)) r
                        (aref data (+ offset 1)) g
                        (aref data (+ offset 2)) b
                        (aref data (+ offset 3)) a)))
              (setf previous pixel)
              (incf index (* run channel-count)))))

(defun decode-stream (stream)
  (with-buffer-read (:stream stream)
    (with-table ()
      (u:mvlet* ((width height channel-count color-space (read-header))
                 (image (make-image width height channel-count color-space)))
        (read-data image channel-count)
        image))))

(defun decode-file (path)
  (u:with-binary-input (stream path)
    (decode-stream stream)))
