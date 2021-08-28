
(asdf:defsystem #:mang.simplest
  :description "Simple commandline UI for Mang"
  :author "Thomas Bartscher <thomas.bartscher@weltraumschlangen.de>"
  :license "BSD-3"
  :depends-on (#:mang
               #:metabang-bind
               #:unix-opts)
  :components
  ((:file "ui/simplest-mang")))
