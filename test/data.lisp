(in-package #:mang)

(defparameter *urwormdwarf-phonemes*
  (glyph-system (map ('c (set "b" "d" "m" "n" "v" "z" "g" "ň" "r" "x"))
                     ('begin (set "b" "d" "m" "n" "v" "g" "ň" "x"))
                     ('end (set "m" "n" "ň"))
                     ('v (set "y" "å" "u")))))

(defparameter *urwormdwarf-words*
  (word-system (list 1 1 `(,(set 'begin nil)
                            v)
                     0 1 `(c v)
                     0 1 `(c v ,(set 'end nil)))
               *urwormdwarf-phonemes*))

(defun generate-urwormdwarf-word ()
  (generate *urwormdwarf-words*
            (map ((constantly t)
                  (uniform-distribution (glyphs<- *urwormdwarf-phonemes*))))))
