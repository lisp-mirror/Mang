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
               #:cl-ppcre-unicode
               #:split-sequence
               #:fgraph
               #:metabang-bind
               )
  :components
  ((:file "package")
   (:module
    "utility"
    :depends-on ("package")
    :components
    ((:file "list")
     (:file "set")
     (:file "distribution")
     (:file "nfsm-dfsm"
            :depends-on ("set" "distribution"))
     (:file "pointer")
     (:file "anaphora")
     (:file "finite-state-transducer")
     (:file "parser-combinator")
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
