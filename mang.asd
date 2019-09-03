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
    ((:file "convenience")
     (:file "constant-functions")
     (:file "anaphora")
     (:file "memoization"
            :depends-on ("anaphora"))
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
    ((:file "globals")
     (:file "blocks")
     (:file "features"
            :depends-on ("globals"
                         "blocks"))
     (:file "glyphs"
            :depends-on ("globals"
                         "blocks"))
     (:file "categories"
            :depends-on ("globals"
                         "blocks"
                         "glyphs"))
     (:file "sonority"
            :depends-on ("blocks"
                         "glyphs"
                         "categories"))
     (:file "gen"
            :depends-on ("globals"
                         "blocks"
                         "glyphs"
                         "categories"))
     (:file "markov"
            :depends-on ("globals"
                         "blocks"
                         "glyphs"
                         "categories"))
     (:file "sound-change"
            :depends-on ("globals"
                         "blocks"
                         "features"
                         "glyphs"
                         "categories"))
     #+nil
     (:module
      "language"
      :components
      ((:file "dictionary")
       (:file "learn"
              :depends-on ("dictionary"))
       (:file "export")
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
