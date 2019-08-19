;;;; mang.asd

(asdf:defsystem #:mang
  :description "Library for generating language families"
  :author "Thomas Bartscher <thomas-bartscher@weltraumschlangen.de>"
  :license "MPL-v2.0"
  :depends-on (#:cells
               #:fset
               #:cl-adt
               #:equal
               #:cl-unicode
               #:split-sequence
               #:metabang-bind
               )
  :components
  ((:file "package")
   (:module
    "utility"
    :depends-on ("package")
    :components
    ((:file "constant-functions")
     (:file "anaphora")
     (:file "set")
     (:file "distribution")
     (:file "nfsm-dfsm"
            :depends-on ("set" "distribution"))
     (:file "pointer")
     (:file "list")
     (:file "finite-state-transducer"
            :depends-on ("constant-functions"
                         "list"))
     (:file "buffered-stream"
            :depends-on ("anaphora"
                         "list"))
     (:file "parser-combinator"
            :depends-on ("anaphora"))
     ))
   (:module
    "source"
    :depends-on ("package" "utility")
    :components
    ((:module
      "phonotactics"
      :components
      ((:file "glyph")
       (:file "word")
       (:file "sonority")
       ))
     (:module
      "grammar"
      :components
      (
       ))
     (:module
      "language"
      :components
      ((:file "dictionary")
       (:file "learn"
              :depends-on ("dictionary"))
       (:file "export")
       ))
     (:module
      "history"
      :components
      ((:file "sound-change")
       ))
     (:module
      "presentation"
      ;; example sentences, glosses and stuff
      :components
      (
       ))
     ))
   #+nil
   (:module
    "test"
    :depends-on ("package" "source")
    :components
    ((:file "urwormdwarf")
     (:file "urkobold")
     (:file "urdokrin")
     (:file "dragon")
     ))
   ))
