(in-package #:qoi)

(defvar *image*)

(u:define-constant +rgb-tag+ #b11111110)
(u:define-constant +rgba-tag+ #b11111111)
(u:define-constant +index-tag+ #b00)
(u:define-constant +diff-tag+ #b01)
(u:define-constant +luma-tag+ #b10)
(u:define-constant +run-tag+ #b11)

(declaim (inline %make-image))
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
  (declare (optimize speed))
  (with-buffer-read (:octets (read-octets 14))
    (unless (string= (the (simple-array character (*)) (read-ascii-string 4)) "qoif")
      (error "Invalid QOI image."))
    (values (read-ub32)
            (read-ub32)
            (read-ub8)
            (read-ub8))))

(declaim (inline read-index))
(defun read-index (octet pixel-table)
  (declare (optimize speed)
           ((u:ub32a (*)) pixel-table))
  (let ((pixel (aref pixel-table (ldb (byte 6 0) octet))))
    (multiple-value-call #'values 1 (unpack-pixel pixel) :index)))

(declaim (inline read-diff))
(defun read-diff (octet pr pg pb pa)
  (declare (optimize speed)
           (u:ub8 pr pg pb pa))
  (u:mvlet ((dr (- (ldb (byte 2 4) octet) 2))
            (dg (- (ldb (byte 2 2) octet) 2))
            (db (- (ldb (byte 2 0) octet) 2)))
    (values 1
            (ldb (byte 8 0) (+ pr dr))
            (ldb (byte 8 0) (+ pg dg))
            (ldb (byte 8 0) (+ pb db))
            pa)))

(declaim (inline read-luma))
(defun read-luma (octet pr pg pb pa)
  (declare (optimize speed)
           (u:ub8 pr pg pb pa))
  (u:mvlet* ((next-octet (read-ub8))
             (dg (- (ldb (byte 6 0) octet) 32))
             (dr-dg (- (ldb (byte 4 4) next-octet) 8))
             (db-dg (- (ldb (byte 4 0) next-octet) 8)))
    (values 1
            (ldb (byte 8 0) (+ pr dg dr-dg))
            (ldb (byte 8 0) (+ pg dg))
            (ldb (byte 8 0) (+ pb dg db-dg))
            pa)))

(declaim (inline read-run))
(defun read-run (octet pr pg pb pa)
  (declare (optimize speed)
           (u:ub8 pr pg pb pa))
  (values (1+ (ldb (byte 6 0) octet)) pr pg pb pa))

(declaim (inline read-chunk))
(defun read-chunk (pixel-table pr pg pb pa)
  (declare (optimize speed))
  (let ((octet (read-ub8)))
    (case octet
      (#.+rgb-tag+
       (let ((pixel (ash (read-ub24) 8)))
         (setf (ldb (byte 8 0) pixel) pa)
         (multiple-value-call #'values 1 (unpack-pixel pixel))))
      (#.+rgba-tag+
       (multiple-value-call #'values 1 (unpack-pixel (read-ub32))))
      (t
       (ecase (ldb (byte 2 6) octet)
         (#.+index-tag+ (read-index octet pixel-table))
         (#.+diff-tag+ (read-diff octet pr pg pb pa))
         (#.+luma-tag+ (read-luma octet pr pg pb pa))
         (#.+run-tag+ (read-run octet pr pg pb pa)))))))

(defun read-data (image pixel-table channel-count)
  (declare (optimize speed)
           (u:ub8 channel-count))
  (loop :with alpha := (= channel-count 4)
        :with index :of-type fixnum := 0
        :with width :of-type u:ub24 := (width image)
        :with height :of-type u:ub24 := (height image)
        :with data := (data image)
        :with pr := #x00
        :with pg := #x00
        :with pb := #x00
        :with pa := #xff
        :while (< index (* width height channel-count))
        :do (u:mvlet ((run r g b a (read-chunk pixel-table pr pg pb pa)))
              (dotimes (i run)
                (setf (aref data (+ index 0)) r
                      (aref data (+ index 1)) g
                      (aref data (+ index 2)) b)
                (when alpha
                  (setf (aref data (+ index 3)) a))
                (incf index channel-count))
              (add-seen-pixel pixel-table r g b a)
              (setf pr r pg g pb b pa a))))

(defun decode-stream (stream)
  (declare (optimize speed))
  (with-buffer-read (:stream stream)
    (let ((pixel-table (u:make-ub32-array 64)))
      (u:mvlet* ((width height channel-count color-space (read-header))
                 (image (make-image width height channel-count color-space)))
        (read-data image pixel-table channel-count)
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
