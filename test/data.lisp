(in-package #:mang)

(defparameter *urwormdwarf-phonemes*
  (glyph-system (map ('c (set "b" "d" "m" "n" "v" "z" "g" "ň" "r" "x"))
                     ('begin (set "b" "d" "m" "n" "v" "g" "ň" "x"))
                     ('end (set "m" "n" "ň" "r" "x"))
                     ('v (set "y" "å" "u")))))

(defparameter *urwormdwarf-words*
  (word-system (set (list `(begin v ,(set 'end nil)))
                    (list 1 1 `(begin v)
                          0 1 `(c v)
                          0 1 `(c v ,(set 'end nil))))
               *urwormdwarf-phonemes*))

(defparameter *urwormdwarf-store*
  (let ((template (set (match-everything-generator)
                       (match-outro-generator 1)
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
                   (set :everything :noun)))

(defparameter *urwormdwarf-verbs*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything :verb)))

(defparameter *urwormdwarf-particles*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything)
                   :negative (set :particle)
                   :learn (set :everything :particle)))

(defparameter *urwormdwarf-adjectives*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything :adjective)))
