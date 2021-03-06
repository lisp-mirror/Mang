(in-package #:mang)

(defun parse-category-definition (glyphs)
  (>>!
    name (parse-identifier)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    phonemes (parse-sequence (<$> (parse-glyph glyphs)
                                  #'second)
                             (>> (parse-whitespace)
                                 (parse-constant ",")
                                 (parse-whitespace)))
    (succeed (map (name phonemes)
                  :default (empty-set)))))

(defun parse-category-definitions (glyphs)
  (parse-lines (parse-category-definition glyphs)
               (empty-map)
               (lambda (glyph glyphs)
                 (map-union glyphs glyph))))

(defun parse-category-section (glyphs)
  (parse-section "categories" (parse-category-definitions glyphs)))

(defun parse-category (categories)
  (// (<~ (parse-from-map categories)
          `(:no-category-found))
      (>>!
        candidate (parse-wrapped (parse-constant "<")
                                 (parse-identifier *mang-reserved-symbols*)
                                 (parse-constant ">"))
        ([av]if (@ categories candidate)
            (succeed `(,candidate ,it))
          (fail `(:unknown-category ,candidate))))))

(defmethod in-category? ((phoneme map)
                         (category sequence))
  (position phoneme category
            :test #'equal?))
