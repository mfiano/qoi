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
  (values (read-uint 3) 1))

(defun read-rgba ()
  (values (read-uint 4) 1))

(defun read-index (octet)
  (let ((pixel (aref *pixel-table* (ldb (byte 6 0) octet))))
    (values pixel 1)))

(defun read-diff (octet previous channel-count)
  (u:mvlet ((r g b a (unpack-pixel previous channel-count))
            (dr (- (ldb (byte 2 4) octet) 2))
            (dg (- (ldb (byte 2 2) octet) 2))
            (db (- (ldb (byte 2 0) octet) 2)))
    (values (pack-pixel (ldb (byte 8 0) (+ r dr))
                        (ldb (byte 8 0) (+ g dg))
                        (ldb (byte 8 0) (+ b db))
                        a
                        channel-count)
            1)))

(defun read-luma (octet previous channel-count)
  (u:mvlet* ((next-octet (read-uint 1))
             (r g b a (unpack-pixel previous channel-count))
             (dg (- (ldb (byte 6 0) octet) 32))
             (dr-dg (- (ldb (byte 4 4) next-octet) 8))
             (db-dg (- (ldb (byte 4 0) next-octet) 8)))
    (values (pack-pixel (ldb (byte 8 0) (+ r dg dr-dg))
                        (ldb (byte 8 0) (+ g dg))
                        (ldb (byte 8 0) (+ b dg db-dg))
                        a
                        channel-count)
            1)))

(defun read-run (octet previous)
  (let ((run (1+ (ldb (byte 6 0) octet))))
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
  (loop :with previous := (case channel-count
                            (3 #x00)
                            (4 #xff))
        :with index := 0
        :with width := (width image)
        :with height := (height image)
        :with data := (data image)
        :while (< index (* width height channel-count))
        :do (u:mvlet* ((pixel run op (read-chunk previous channel-count))
                       (r g b a (unpack-pixel pixel channel-count)))
              (add-seen-pixel r g b a channel-count)
              (dotimes (i run)
                (setf (aref data (+ index 0)) r
                      (aref data (+ index 1)) g
                      (aref data (+ index 2)) b)
                (when (= channel-count 4)
                  (setf (aref data (+ index 3)) a))
                (incf index channel-count))
              (setf previous pixel))))

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

(defun convert-to-png (image file-path)
  (let* ((width (width image))
         (height (height image))
         (channel-count (case (channels image)
                          (:rgb 3)
                          (:rgba 4)))
         (image-data (u:make-ub8-array (* width height channel-count)))
         (png (make-instance 'zpng:png
                             :color-type (ecase channel-count
                                           (3 :truecolor)
                                           (4 :truecolor-alpha))
                             :width width
                             :height height
                             :image-data image-data)))
    (replace image-data (data image))
    (zpng:write-png png file-path)))
