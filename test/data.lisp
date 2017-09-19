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

(defun generate-urwormdwarf-word ()
  (generate *urwormdwarf-words*
            (map ((constantly t)
                  (uniform-distribution (glyphs<- *urwormdwarf-phonemes*))))))

(defun urwormdwarf-word-set (&optional (n 100))
  (convert 'set
           (loop :for x :below n
              :collect (generate-urwormdwarf-word))))
