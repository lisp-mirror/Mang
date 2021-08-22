;;;; mang.asd

(asdf:defsystem #:mang
  :description "Library for generating language families"
  :author "Thomas Bartscher <thomas.bartscher@weltraumschlangen.de>"
  :depends-on (#:fset
               #:cl-adt
               #:cl-unicode
               #:split-sequence
               #:metabang-bind
               #:cells
               #:cl-bus)
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
     (:file "set"
      :depends-on ("list"))
     (:file "distribution"
      :depends-on ("anaphora"))
     (:file "nfsm-dfsm"
      :depends-on ("anaphora" "set" "distribution"))
     (:file "pointer")
     (:file "list"
      :depends-on ("anaphora"))
     (:file "finite-state-transducer"
      :depends-on ("anaphora" "constant-functions" "list"))
     (:file "parser-combinator"
            :depends-on ("anaphora"))
     (:file "dot")
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
     (:file "word"
      :depends-on ("globals"
                   "blocks"
                   "glyphs"
                   "gen"
                   "markov"))
     (:file "language"
      :depends-on ("markov"))
     (:file "dictionary"
      :depends-on ("word"
                   "language"))
     (:file "sound-change"
      :depends-on ("globals"
                   "blocks"
                   "features"
                   "glyphs"
                   "categories"
                   "language"))
     (:file "semantic-change"
      :depends-on ("language"
                   "dictionary"
                   "sound-change"))
     (:file "read-mang"
      :depends-on ("features"
                   "language"
                   "dictionary"))
     ))
   ))
