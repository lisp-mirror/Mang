;;;; mang.asd

(asdf:defsystem #:mang
  :description "Describe mang here"
  :author "Thomas Bartscher <thomas.bartscher@weltraumschlangen.de>"
  :license "BSD3"
  :depends-on (#:equal
               #:cl-adt
               #:rope)
  :components
  ((:file "package")
   (:module
    "source"
    :depends-on ("package")
    :components
    ((:file "distribution")
     (:file "match")
     (:file "chop"
            :depends-on ("distribution"
                         "match"))
     (:file "follow-db"
            :depends-on ("distribution"
                         "match"))
     ))
   ))
