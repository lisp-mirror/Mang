;;;; mang.asd

(asdf:defsystem #:mang
  :description "Library for generating language families"
  :author "Thomas Bartscher <thomas-bartscher@weltraumschlangen.de>"
  :depends-on (#:fset
               #:cl-adt
               #:cl-unicode
               #:split-sequence
               #:metabang-bind
               #:unix-opts
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
     (:file "set"
      :depends-on ("list"))
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
     (:file "word"
      :depends-on ("globals"
                   "blocks"
                   "glyphs"
                   "gen"
                   "markov"))
     (:file "language"
      :depends-on ("word"))
     (:file "read-mang"
      :depends-on ("blocks"
                   "features"
                   "glyphs"
                   "categories"
                   "sonority"
                   "gen"
                   "markov"
                   "word"
                   "language"))
     ))
   ))
