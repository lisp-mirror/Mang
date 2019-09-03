(in-package #:mang)

(defun parse-word (glyphs)
  (some (parse-glyph glyphs)
        '() #'cons))

(defun parse-gloss (glyphs)
  (>>!
    gloss (parse-identifier *mang-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    word (parse-word glyphs)
    (succeed (map (gloss word)))))
