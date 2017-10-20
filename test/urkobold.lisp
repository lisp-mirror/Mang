(in-package #:mang)

(defparameter *urkobold-phonemes*
  (glyph-system (map ('c (set "p" "b" "t" "d" "k" "g" "m" "n" "ŋ" "ɸ" "β" "s"
                              "z" "ʃ" "ʒ" "ɕ" "ʑ" "ɹ" "l"))
                     ('v (set "i" "y" "u" "e" "ɵ" "o" "ə" "a"))
                     ('t (set "p" "t" "k"))
                     ('d (set "b" "d" "g"))
                     ('p (set "p" "b"))
                     ('s (set "ɸ" "ʃ" "s" "ɕ" "ɹ" "l"))
                     ('z (set "β" "z" "ʒ" "ʑ" "ɹ" "l"))
                     ('l (set "ɹ" "l"))
                     ('r (set "t" "d" "n" "ɹ" "l"))
                     ('n (set "m" "n" "ŋ"))
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
  (let ((dist (uniform-distribution (glyphs<- *urkobold-phonemes*))))
    (image (lambda (k v)
             (values k (learner v dist)))
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
                      (match-outro-generator
                       1 :ignore-glyphs
                       (set ($ (@ (classes<- *urkobold-phonemes*)
                                  'v))
                            "^"))))
                (:beautiful
                 (set (match-everything-generator)
                      (match-outro-generator
                       1 :ignore-glyphs
                       (set ($ (@ (classes<- *urkobold-phonemes*)
                                  'c))
                            "^"))
                      (match-outro-generator 1 :ignore-glyphs
                                             (@ (classes<- *urkobold-phonemes*)
                                                'v))))
                (:light
                 (set (match-everything-generator)
                      (match-outro-generator 1 :ignore-glyphs (set "" "^"))))
                (:dark
                 (set (match-everything-generator)
                      (match-outro-generator 1 :ignore-glyphs (set "" "^"))))))))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :good :beautiful :light)
                        :negative (set :verb :adjective :bad :ugly :dark)
                        :learn
                        (set :count :everything :noun :good :beautiful :light))
       ;; sun
       '("g" "ʑ" "a" "" "l" "^" "a"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :good :beautiful :dark)
                        :negative (set :verb :adjective :bad :ugly :light)
                        :learn
                        (set :count :everything :noun :good :beautiful :dark))
       ;; moon
       '("o" "ɹ" "n"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :practical)
                        :negative (set :verb :adjective :bad)
                        :learn (set :count :everything :noun :practical))
       ;; earth/ground/soil
       '("l" "^" "ɵ" "" "d" "ə"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :plant)
                        :negative (set :verb :adjective)
                        :learn (set :count :everything :noun :plant))
       ;; plant
       '("p" "l" "^" "a"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything :noun :plant)
                        :negative (set :verb :adjective :bad :ugly)
                        :learn (set :count :everything :noun :plant))
       ;; tree
       '("e" "l" "n"))


(learn (learning-markov (& *urkobold-store*)
                        (set :everything)
                        :negative (set :count :noun :verb :adjective :affix)
                        :learn (set :count :everything :affix))
       ;; plural – suffix
       '("g" "l" "y"))

(learn (learning-markov (& *urkobold-store*)
                        (set :everything)
                        :negative (set :count :noun :verb :adjective :affix)
                        :learn (set :count :everything :affix))
       ;;
       '())
