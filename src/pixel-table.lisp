(in-package #:qoi)

(declaim (inline pack-pixel))
(defun pack-pixel (r g b a)
  (declare (optimize speed))
  (let ((pixel 0))
    (setf (ldb (byte 8 24) pixel) r
          (ldb (byte 8 16) pixel) g
          (ldb (byte 8 8) pixel) b
          (ldb (byte 8 0) pixel) a)
    pixel))

(declaim (inline unpack-pixel))
(defun unpack-pixel (pixel)
  (declare (optimize speed))
  (values (ldb (byte 8 24) pixel)
          (ldb (byte 8 16) pixel)
          (ldb (byte 8 8) pixel)
          (ldb (byte 8 0) pixel)))

(declaim (inline hash-pixel))
(defun hash-pixel (r g b a)
  (declare (optimize speed)
           (u:ub8 r g b a))
  (mod (+ (* r 3) (* g 5) (* b 7) (* a 11)) 64))

(declaim (inline add-seen-pixel))
(defun add-seen-pixel (table r g b a)
  (declare (optimize speed)
           ((u:ub32a (64)) table))
  (let ((index (hash-pixel r g b a)))
    (setf (aref table index) (pack-pixel r g b a))))
