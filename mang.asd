;;;; mang-cells.asd

(asdf:defsystem #:mang
  :description "Describe mang-cells here"
  :author "Thomas Bartscher <thomas-bartscher@weltraumschlangen.de>"
  :license "MPL-v2.0"
  :depends-on (#:cells
               #:fset
               #:cl-adt
               #:equal
               #:cl-ppcre-unicode)
  :components
  ((:file "package")
   (:module
    "utility"
    :depends-on ("package")
    :components
    ((:file "set")
     (:file "distribution")
     (:file "nfsm-dfsm"
            :depends-on ("set" "distribution"))
     ))
   (:module
    "source"
    :depends-on ("package" "utility")
    :components
    ((:module
      "phonotactics"
      :components
      (
       ))
     (:module
      "grammar"
      :components
      (
       ))
     (:module
      "language"
      :components
      (
       ))
     (:module
      "history"
      :components
      (
       ))
     ))
   ))
