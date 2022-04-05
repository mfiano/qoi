(in-package #:cl-qoi)

(defvar *pixel-table*)

(defmacro with-table (() &body body)
  `(let ((*pixel-table* (u:make-ub32-array 64)))
     ,@body))

(defun pack-pixel (r g b a channel-count)
  (let ((pixel 0))
    (setf (ldb (byte 8 (* 8 (- channel-count 1))) pixel) r
          (ldb (byte 8 (* 8 (- channel-count 2))) pixel) g
          (ldb (byte 8 (* 8 (- channel-count 3))) pixel) b)
    (when (= channel-count 4)
      (setf (ldb (byte 8 (* 8 (- channel-count 4))) pixel) a))
    pixel))

(defun unpack-pixel (pixel channel-count)
  (values (ldb (byte 8 (* 8 (- channel-count 1))) pixel)
          (ldb (byte 8 (* 8 (- channel-count 2))) pixel)
          (ldb (byte 8 (* 8 (- channel-count 3))) pixel)
          (if (= channel-count 4)
              (ldb (byte 8 (* 8 (- channel-count 4))) pixel)
              255)))

(defun hash-pixel (r g b a)
  (mod (+ (* r 3) (* g 5) (* b 7) (* a 11)) 64))

(defun add-seen-pixel (r g b a channel-count)
  (let ((index (hash-pixel r g b a)))
    (setf (aref *pixel-table* index) (pack-pixel r g b a channel-count))))
