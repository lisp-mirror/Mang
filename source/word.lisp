(in-package #:mang)

(defun parse-word (glyphs)
  (declare (type map glyphs))
  (<$> (some (<$> (parse-glyph glyphs)
                  #'second)
             '() #'cons)
       (lambda (word)
         (append `(,(map (:begin t)))
                 word `(,(map (:end t)))))))

(defun string<-word (glyphs word
                   &key
                     (computer-readable? t))
  (reduce (lambda (string phoneme)
            (concatenate 'string
                         string (if computer-readable?
                                    "<"
                                    "")
                         (arb (origin phoneme glyphs))
                         (if computer-readable?
                             ">"
                             "")))
          (butlast (rest word))
          :initial-value ""))
