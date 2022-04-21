(asdf:defsystem #:qoi
  :description "A decoder for the Quite OK Image format."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/qoi"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on
  (#:babel
   #:fast-io
   #:mfiano-utils
   #:zpng)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "buffer")
   (:file "pixel-table")
   (:file "qoi")))
