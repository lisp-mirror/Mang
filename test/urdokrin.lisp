(in-package #:mang)

(defparameter *urdokrin-phonemes*
  (glyph-system (map ('c (set "p" "t" "k" "b" "d" "g"
                              "ph" "th" "kh" "bh" "dh" "gh"
                              "m" "n" "nh" "f" "c" "x" "v" "j" "r"))
                     ('m (set "m" "n" "nh" "v" "j" "r"))
                     ('v (set "i" "e" "a" "u" "o" "ô" "ôe" "ai" "au" "ao" "oi"
                              "ou" "oa" "oô" "ei" "ui" "uô" "ie" "io")))))

(defparameter *urdokrin-words*
  (word-system (list 1 3 `(c v ,(set 'm nil)))
               *urdokrin-phonemes*))

(defparameter *urdokrin-store*
  (let ((dist (uniform-distribution (glyphs<- *urdokrin-phonemes*)))
        (consonants (set ($ (@ (classes<- *urdokrin-phonemes*)
                               'c))
                         ""))
        (vowels (set ($ (@ (classes<- *urdokrin-phonemes*)
                           'v))
                     "")))
    (store (image (lambda (category template)
                    (values category (learner template dist)))
                  (map
                   (:count
                    (set (match-everything-generator)))
                   (:everything
                    (set (match-outro-generator 4)
                         (match-outro-generator 2 :ignore-glyphs consonants)))
                   (:mobile
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:material
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   (:immobile
                    (set (match-outro-generator 3)
                         (match-outro-generator 2 :ignore-glyphs (set ""))
                         (match-outro-generator 1 :ignore-glyphs consonants)
                         (match-outro-generator 1 :ignore-glyphs vowels)))
                   )))))

(defparameter *urdokrin-dictionary*
  (dictionary))

(defun learn-urdokrin-word (form gloss learn)
  (let ((word (word form)))
    (unless (run-dfsm *urdokrin-words* form)
      (error "Form ~S is not a valid urdokrin word"
             form))
    (unless (empty? (@ *urdokrin-dictionary* form))
      (warn "Homophones for ~S:~% ~S ~S~%~{~{ ~S~}~}"
            form gloss learn
            (convert 'list
                     (image (lambda (entry)
                              (list (gloss<- entry)
                                    (learn<- entry)))
                            (@ *urdokrin-dictionary* form)))))
    (learn-word *urdokrin-store* word learn)
    (setf *urdokrin-dictionary*
          (with *urdokrin-dictionary*
                (dictionary-entry word gloss learn)))))

(defun generate-urdokrin-word (categories &optional (negative (set)))
  (generate-word *urdokrin-words* *urdokrin-store* categories
                 :negative negative))

;;;; nouns
(learn-urdokrin-word '("d" "o" "" "kh" "ie" "nh")
                     "person"
                     (set :count :everything :mobile))

(learn-urdokrin-word '("k" "u" "" "c" "a")
                     "loam"
                     (set :count :everything :material))

(learn-urdokrin-word '("th" "a" "" "m" "o")
                     "clay"
                     (set :count :everything :material))

(learn-urdokrin-word '("dh" "i" "" "kh" "o")
                     "ceramic"
                     (set :count :everything :material))

(learn-urdokrin-word '("bh" "o" "" "d" "au")
                     "brick"
                     (set :count :everything :mobile))

(learn-urdokrin-word '("bh" "a" "" "m" "ôe")
                     "animal"
                     (set :count :everything :mobile))

(learn-urdokrin-word '("m" "ei" "" "d" "a")
                     "fresh cadaver"
                     (set :count :everything :mobile))

(learn-urdokrin-word '("m" "ie" "n" "" "b" "i")
                     "ripe cadaver"
                     (set :count :everything :mobile))

(learn-urdokrin-word '("t" "ôe" "" "n" "io")
                     "corpse"
                     (set :count :everything :mobile))

(learn-urdokrin-word '("m" "ie" "" "c" "ao")
                     "food"
                     (set :count :everything :material))

(learn-urdokrin-word '("th" "a" "n" "" "dh" "a")
                     "herb"
                     (set :count :everything :immobile))

(learn-urdokrin-word '("v" "uô" "m" "" "t" "ui")
                     "weed"
                     (set :count :everything :immobile))

(learn-urdokrin-word '("j" "u" "" "bh" "uô")
                     "tree"
                     (set :count :everything :immobile))
