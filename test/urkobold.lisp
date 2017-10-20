(in-package #:mang)

(defparameter *urkobold-phonemes*
  (glyph-system (map ('c (set "p" "b" "ʈ" "ɖ" "k" "g" "m" "ɳ" "ŋ" "ɸ" "β" "s"
                              "z" "ʂ" "ʐ" "ɕ" "ʑ" "ɻ" "ɭ"))
                     ('v (set "i" "y" "u" "e" "ɵ" "o" "ə" "a"))
                     ('t (set "p" "ʈ" "k"))
                     ('d (set "b" "ɖ" "g"))
                     ('p (set "p" "b"))
                     ('s (set "ɸ" "ʂ" "s" "ɕ" "ɻ" "ɭ"))
                     ('z (set "β" "z" "ʐ" "ʑ" "ɻ" "ɭ"))
                     ('l (set "ɻ" "ɭ"))
                     ('r (set "ɳ" "ɻ" "ɭ"))
                     ('n (set "m" "ɳ" "ŋ"))
                     ('tongue (set "^")))))

(defparameter *urkobold-words*
  (word-system (set (list 1 4 (set `(c v)
                                   `(t s v)
                                   `(d z v)
                                   `(v l n)
                                   `(r tongue v)
                                   `(p l tongue v))))
               *urkobold-phonemes*))

(defparameter *urkobold-store*
  (map (:count
        (set (match-everything-generator)))
       (:everything
        (set (match-outro-generator 2)
             (match-outro-generator 3 :ignore-glyphs (set ""))))
       (:noun
        (set (match-outro-generator 1)
             (match-outro-generator 2 :ignore-glyphs (set ""))))
       (:verb
        (set (match-outro-generator 1)
             (match-outro-generator 2)
             (match-outro-generator 2 :ignore-glyphs (set "" "^"))))
       (:adjective
        (set (match-outro-generator 2 :ignore-glyphs (set "^"))))
       (:affix
        (set (match-everything-generator)
             (match-outro-generator 1 :ignore-glyphs (set ""))
             (match-outro-generator 2)))
       (:good
        (set (match-everything-generator)
             (match-outro-generator 1)
             (match-outro-generator 3 :ignore-glyphs (set ""))))
       (:bad
        (set (match-everything-generator)
             (match-outro-generator 2 :ignore-glyphs (set ""))
             (match-outro-generator 3)))
       (:funny
        (set (match-everything-generator)
             (match-outro-generator 1)
             (match-outro-generator 2 :ignore-glyphs (set "" "^"))))
       (:sad
        (set (match-everything-generator)
             (match-outro-generator 1)
             (match-outro-generator 2)
             (match-outro-generator 3 :ignore-glyphs
                                    (@ (classes<- *urkobold-phonemes*)
                                       'v))))
       (:practical
        (set (match-everything-generator)
             (match-outro-generator 3 :ignore-glyphs (set ""))
             (match-outro-generator 2 :ignore-glyphs
                                    (@ (classes<- *urkobold-phonemes*)
                                       'c))))
       (:plant
        (set (match-outro-generator 2)))
       (:animal
        (set (match-outro-generator 2)))
       (:ugly
        (set (match-everything-generator)
             (match-outro-generator 1 :ignore-glyphs
                                    (@ (classes<- *urkobold-phonemes*)
                                       'c))
             (match-outro-generator 1 :ignore-glyphs
                                    (set ($ (@ (classes<- *urkobold-phonemes*)
                                               'v))
                                         "^"))))
       (:beautiful
        (set (match-everything-generator)
             (match-outro-generator 1 :ignore-glyphs
                                    (set ($ (@ (classes<- *urkobold-phonemes*)
                                               'c))
                                         "^"))
             (match-outro-generator 1 :ignore-glyphs
                                    (@ (classes<- *urkobold-phonemes*)
                                       'v))))))
