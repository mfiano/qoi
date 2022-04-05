(asdf:defsystem #:cl-qoi
  :description "An encoder/decoder for the Quite OK Image format."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://github.com/mfiano/cl-qoi"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on (#:babel
               #:fast-io
               #:mfiano.misc.utils
               #:zpng)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "buffer")
   (:file "pixel-table")
   (:file "qoi")))
