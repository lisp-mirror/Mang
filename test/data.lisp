(in-package #:mang)

(defparameter *urwormdwarf-phonemes*
  (glyph-system (map ('c (set "b" "d" "m" "n" "v" "z" "g" "ň" "r" "x"))
                     ('begin (set "b" "d" "m" "n" "v" "g" "ň" "x"))
                     ('end (set "m" "n" "ň"))
                     ('v (set "y" "å" "u")))))

(defparameter *urwormdwarf-words*
  (word-system (set (list `(begin v ,(set 'end nil)))
                    (list 1 1 `(begin v)
                          0 1 `(c v)
                          0 1 `(c v ,(set 'end nil))))
               *urwormdwarf-phonemes*))

(defparameter *falranda-learner*
  (learner (map ((constantly t)
                 (uniform-distribution (with (glyphs<- *urwormdwarf-phonemes*)
                                             "")
                                       100)))
           (set (match-outro-generator 3 (complement (set "")))
                (match-outro-generator 3)
                (match-outro-generator 4)
                (match-outro-generator 5)
                (match-outro-generator 2)
                (match-outro-generator 1)
                (match-everything-generator))))
