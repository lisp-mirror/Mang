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
