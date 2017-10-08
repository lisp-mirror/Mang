(in-package #:mang)

;;;; Dwarf data
(defparameter *urwormdwarf-phonemes*
  (glyph-system (map ('c (set "b" "d" "m" "n" "v" "z" "g" "ň" "r"))
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
                   (set :everything :noun)))
(learn *urwormdwarf-nouns* '("d" "å" "" "m" "y"))

(defparameter *urwormdwarf-verbs*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything :verb)))
(learn *urwormdwarf-verbs* '("m" "å" "x"))
(learn *urwormdwarf-verbs* '("m" "u" "m"))
(learn *urwormdwarf-verbs* '("v" "y" "" "z" "u" "n"))
(learn *urwormdwarf-verbs* '("b" "å" "" "g" "u" "" "r" "u" "ň"))

(defparameter *urwormdwarf-particles*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything)
                   :negative (set :particle)
                   :learn (set :everything :particle)))
(learn *urwormdwarf-particles* '("d" "y" "r"))
(learn *urwormdwarf-particles* '("m" "y" "r"))
(learn *urwormdwarf-particles* '("d" "u"))
(learn *urwormdwarf-particles* '("ň" "u" "n"))
(learn *urwormdwarf-particles* '("ň" "y"))

(defparameter *urwormdwarf-numbers*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything)
                   :negative (set :number :particle)
                   :learn (set :everything :number)))
(learn *urwormdwarf-numbers* '("b" "å"))
(learn *urwormdwarf-numbers* '("v" "y" "m"))
(learn *urwormdwarf-numbers* '("g" "u" "x"))
(learn *urwormdwarf-numbers* '("m" "å" "" "r" "u"))
(learn *urwormdwarf-numbers* '("g" "å" "" "m" "å" "ň"))
(learn *urwormdwarf-numbers* '("b" "y" "" "d" "u" "m"))
(learn *urwormdwarf-numbers* '("ň" "å" "ň"))

(defparameter *urwormdwarf-adjectives*
  (learning-markov (& *urwormdwarf-store*)
                   (set :everything :adjective)))

;;;; Kobold data – need to be revised
(defparameter *urkobold-phonemes*
  (glyph-system (map ('p (set #+nil"p" "p"
                              #+nil"b" "b"
                              #+nil"c" "c"
                              #+nil"ɟ" "c̬"
                              #+nil"k" "k"
                              #+nil"ɡ" "g"))
                     ('n (set #+nil"m" "m"
                              #+nil"ɳ" "n"))  ; allophone "n"
                     ('f (set #+nil"ɸ" "f"
                              #+nil"β" "v"
                              #+nil"ʃ" "ŝ"
                              #+nil"ʒ" "ẑ"
                              #+nil"ç" "ś"
                              #+nil"ʝ" "ź"
                              #+nil"h" "h"))
                     ('v (set #+nil"i" "i"
                              #+nil"e" "e"
                              #+nil"u" "u"
                              #+nil"o" "o"
                              #+nil"a" "a"
                              #+nil"ə" "ê"))
                     ('w (set #+nil"ɻ̩" "r"
                              #+nil"l̩" "l"
                              #+nil"j̩" "j"
                              #+nil"w̩" "w")))))

(defparameter *urkobold-words*
  (word-system (set #+nil
                    (list 1 1 (set `(p ,(set 'f nil)
                                       v f)))
                    (list 1 1 (set `(p ,(set 'f nil)
                                       v ,(set 'f 'n))
                                   `(p w n)))
                    (list 0 1 (set `(p ,(set 'f nil)
                                       v ,(set 'f 'n))
                                   `(n v ,(set 'f 'n))
                                   `(p w n))
                          1 1 (set `(v p f)
                                   `(,(set 'v 'w)
                                      n))
                          0 1 (set `(v f)
                                   `(w n)))
                    #+nil
                    (list 1 1 (set `(p ,(set 'f nil)
                                       v)
                                   `(p w))
                          0 1 (set `(c v)
                                   `(p w))
                          0 1 (set `(c v f)
                                   `(p w ,(set 'p nil)))))
               *urkobold-phonemes*))

(defparameter *urkobold-store*
  (let ((template (set (match-everything-generator)
                       (match-outro-generator 1)
                       (match-outro-generator 2 :ignore-glyphs (set ""))
                       (match-outro-generator 3 :ignore-glyphs (set ""))
                       (match-outro-generator 4 :ignore-glyphs (set ""))))
        (dist (uniform-distribution (glyphs<- *urkobold-phonemes*))))
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

(defparameter *urkobold-nouns*
  (learning-markov (& *urkobold-store*)
                   (set :everything :noun)))

(defparameter *urkobold-verbs*
  (learning-markov (& *urkobold-store*)
                   (set :everything :verb)))

(defparameter *urkobold-particles*
  (learning-markov (& *urkobold-store*)
                   (set :everything)
                   :negative (set :particle)
                   :learn (set :everything :particle)))

(defparameter *urkobold-numbers*
  (learning-markov (& *urkobold-store*)
                   (set :everything)
                   :negative (set :number)
                   :learn (set :everything :number)))

(defparameter *urkobold-adjectives*
  (learning-markov (& *urkobold-store*)
                   (set :everything :adjective)))
