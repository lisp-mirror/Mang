(in-package #:mang)

;;;; Dwarf data
(defparameter *urwormdwarf-phonemes*
  (glyph-system (map ('c (set "b" "d" "m" "n" "v" "z" "g" "ň" "r"))
                     ('begin (set "b" "d" "m" "n" "v" "g" "ň" "x"))
                     ('end (set "m" "n" "ň" "r" "x"))
                     ('v (set "y" "å" "u"))
                     ('cv (set "ÿ" "o" "ü")))))

(defparameter *urwormdwarf-words*
  (word-system (set (list `(begin ,(set 'v 'cv)
                                  ,(set 'end nil)))
                    (list `(begin ,(set 'v 'cv))
                          0 1 `(c v ,(set 'end nil)))
                    (list `(begin ,(set 'v 'cv))
                          `(c v)
                          0 1 `(c ,(set 'v 'cv)
                                  ,(set 'end nil))))
               *urwormdwarf-phonemes*))

#|
(defparameter *urwormdwarf-store*
  (let ((template (set (match-everything-generator)
                       (match-outro-generator 1)
                       (match-outro-generator
                        1 :ignore-glyphs
                        (set ($ (@ (classes<- *urwormdwarf-phonemes*)
                                   'c))
                             ""))
                       (match-outro-generator
                        1 :ignore-glyphs
                        (set ($ (@ (classes<- *urwormdwarf-phonemes*)
                                   'v))
                             ""))
                       (match-outro-generator 2 :ignore-glyphs (set ""))
                       (match-outro-generator 3 :ignore-glyphs (set ""))
                       (match-outro-generator 4 :ignore-glyphs (set ""))))
        (dist (uniform-distribution (glyphs<- *urwormdwarf-phonemes*))))
    (map (:noun
          (learner template dist))
         (:verb
          (learner template dist))
         (:particle
          (learner template dist))
         (:number
          (learner template dist))
         (:adjective
          (learner template dist))
         (:everything
          (learner template dist))
         (:beautiful
          (learner template dist))
         (:ugly
          (learner template dist)))))

(defparameter *urwormdwarf-nouns*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything :noun)
                   :negative (set :verb)))
(learn *urwormdwarf-nouns* '("n" "y" "" "z" "å" "" "v" "u"))
(learn *urwormdwarf-nouns* '("x" "å" "" "m" "y" "m"))
(learn *urwormdwarf-nouns* '("x" "ü" "" "v" "u" "m"))
(learn *urwormdwarf-nouns* '("d" "å" "" "m" "y"))
(learn *urwormdwarf-nouns* '("x" "ü" "" "v" "y" "r"))
(learn *urwormdwarf-nouns* '("m" "u" "" "x" "å"))
(learn *urwormdwarf-nouns* '("b" "å" "" "r" "y" "ň"))
(learn *urwormdwarf-nouns* '("v" "å" "" "ň" "å"))
(learn *urwormdwarf-nouns* '("d" "u" "" "v" "y" "r"))
(learn *urwormdwarf-nouns* '("x" "å" "" "b" "å" "x"))
(learn *urwormdwarf-nouns* '("g" "o" "" "m" "å" "" "d" "u" "m"))
(learn *urwormdwarf-nouns* '("v" "å" "" "m" "u" "" "d" "u" "m"))
(learn *urwormdwarf-nouns* '("ň" "u" "" "r" "u" "r"))
(learn *urwormdwarf-nouns* '("m" "ÿ" "" "g" "å" "m"))
(learn *urwormdwarf-nouns* '("m" "å" "" "ň" "å" "r"))
(learn *urwormdwarf-nouns* '("m" "u" "" "r" "u" "x"))
(learn *urwormdwarf-nouns* '("x" "å" "" "z" "y"))
(learn *urwormdwarf-nouns* '("v" "y" "r"))
(learn *urwormdwarf-nouns* '("m" "y" "" "b" "å"))

(defparameter *urwormdwarf-verbs*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything :verb)
                   :negative (set :noun)))
(learn *urwormdwarf-verbs* '("d" "u" "" "g" "å" "r"))
(learn *urwormdwarf-verbs* '("m" "u" "" "b" "å" "x"))
(learn *urwormdwarf-verbs* '("ň" "å" "" "d" "y" "" "z" "u" "n"))
(learn *urwormdwarf-verbs* '("m" "ÿ" "r"))
(learn *urwormdwarf-verbs* '("v" "y" "" "g" "å" "r"))
(learn *urwormdwarf-verbs* '("m" "o" "" "v" "å" "m"))
(learn *urwormdwarf-verbs* '("b" "ÿ" "" "g" "å" "x"))
(learn *urwormdwarf-verbs* '("d" "u" "r"))
(learn *urwormdwarf-verbs* '("g" "u" "" "z" "y" "x"))
(learn *urwormdwarf-verbs* '("m" "o" "x"))
(learn *urwormdwarf-verbs* '("m" "u" "m"))
(learn *urwormdwarf-verbs* '("v" "ÿ" "" "z" "u" "n"))
(learn *urwormdwarf-verbs* '("b" "å" "" "g" "u" "" "r" "y" "ň"))
(learn *urwormdwarf-verbs* '("b" "y" "x"))
(learn *urwormdwarf-verbs* '("v" "å" "" "x" "u" "" "r" "y" "ň"))
(learn *urwormdwarf-verbs* '("d" "u" "" "d" "u" "" "m" "y"))
(learn *urwormdwarf-verbs* '("d" "u" "" "v" "å" "" "g" "o" "r"))
(learn *urwormdwarf-verbs* '("m" "o" "" "g" "u"))
(learn *urwormdwarf-verbs* '("g" "u" "" "r" "y" "x"))
(learn *urwormdwarf-verbs* '("m" "å" "" "d" "u" "m"))
(learn *urwormdwarf-verbs* '("b" "u" "r"))
(learn *urwormdwarf-verbs* '("v" "u" "" "d" "u" "m"))
(learn *urwormdwarf-verbs* '("x" "u" "" "g" "å" "" "r" "y" "ň"))
(learn *urwormdwarf-verbs* '("ň" "o" "" "d" "u" "" "r" "u" "r"))
(learn *urwormdwarf-verbs* '("d" "u" "" "m" "y" "x"))
(learn *urwormdwarf-verbs* '("x" "å" "" "r" "y" "ň"))
(learn *urwormdwarf-verbs* '("x" "å" "n"))

(defparameter *urwormdwarf-particles*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything)
                   :negative (set :everything :particle :number)
                   :learn (set :everything :particle)))
(learn *urwormdwarf-particles* '("v" "å"))
(learn *urwormdwarf-particles* '("x" "y"))
(learn *urwormdwarf-particles* '("ň" "u"))
(learn *urwormdwarf-particles* '("ň" "å" "x"))

(learn *urwormdwarf-particles* '("d" "u"))
(learn *urwormdwarf-particles* '("ň" "u" "n"))
(learn *urwormdwarf-particles* '("d" "u" "m"))
(learn *urwormdwarf-particles* '("ň" "y"))
(learn *urwormdwarf-particles* '("b" "y" "m"))
(learn *urwormdwarf-particles* '("d" "y" "r"))
(learn *urwormdwarf-particles* '("x" "u" "r"))
(learn *urwormdwarf-particles* '("m" "u" "" "r" "y" "ň"))
(learn *urwormdwarf-particles* '("d" "o" "" "b" "å"))
(learn *urwormdwarf-particles* '("b" "o" "" "m" "å" "ň"))
(learn *urwormdwarf-particles* '("x" "å"))
(learn *urwormdwarf-particles* '("x" "y" "ň"))
(learn *urwormdwarf-particles* '("g" "å" "r"))
(learn *urwormdwarf-particles* '("v" "u" "" "r" "u"))
(learn *urwormdwarf-particles* '("m" "å" "" "g" "å" "m"))
(learn *urwormdwarf-particles* '("v" "u" "" "ň" "u" "r"))
(learn *urwormdwarf-particles* '("v" "u"))
(learn *urwormdwarf-particles* '("n" "å"))
(learn *urwormdwarf-particles* '("v" "å" "m"))
(learn *urwormdwarf-particles* '("n" "ü" "r"))

(defparameter *urwormdwarf-numbers*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything)
                   :negative (set :number :particle)
                   :learn (set :everything :number)))
(learn *urwormdwarf-numbers* '("b" "å"))
(learn *urwormdwarf-numbers* '("v" "y" "m"))
(learn *urwormdwarf-numbers* '("g" "ü" "x"))
(learn *urwormdwarf-numbers* '("ň" "å" "ň"))
(learn *urwormdwarf-numbers* '("m" "å" "" "r" "u"))
(learn *urwormdwarf-numbers* '("g" "o" "" "m" "å" "ň"))
(learn *urwormdwarf-numbers* '("b" "y" "" "d" "u" "m"))
(learn *urwormdwarf-numbers* '("m" "ÿ" "x"))

(defparameter *urwormdwarf-adjectives*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything :adjective :number)))
(learn *urwormdwarf-adjectives* '("n" "ÿ" "" "m" "u" "r"))
(learn *urwormdwarf-adjectives* '("ň" "å" "" "d" "u"))
(learn *urwormdwarf-adjectives* '("x" "ÿ" "" "m" "y" "n"))
(learn *urwormdwarf-adjectives* '("v" "å" "" "m" "u" "" "m" "y" "x"))
(learn *urwormdwarf-adjectives* '("v" "u" "" "d" "å"))
(learn *urwormdwarf-adjectives* '("v" "ÿ" "x"))
(learn *urwormdwarf-adjectives* '("n" "u" "" "v" "u" "x"))
(learn *urwormdwarf-adjectives* '("g" "u" "" "ň" "å" "m"))
(learn *urwormdwarf-adjectives* '("d" "u" "" "n" "å" "x"))
(learn *urwormdwarf-adjectives* '("x" "å" "" "r" "u" "r"))
(learn *urwormdwarf-adjectives* '("d" "u" "" "r" "u"))
(learn *urwormdwarf-adjectives* '("x" "å" "" "m" "y" "ň"))
(learn *urwormdwarf-adjectives* '("v" "y" "" "z" "y" "" "r" "o" "m"))
(learn *urwormdwarf-adjectives* '("d" "u" "" "m" "å" "" "g" "å"))
(learn *urwormdwarf-adjectives* '("v" "y" "x"))
(learn *urwormdwarf-adjectives* '("x" "å" "" "ň" "å" "r"))
|#
