(in-package #:mang)

(defun parse-category-definition (glyphs)
  (>>!
    name (parse-identifier)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    phonemes (parse-separated (parse-glyph glyphs)
                              "," (empty-set)
                              (lambda (glyph glyphs)
                                (with glyphs (cdr glyph))))
    (succeed (map (name phonemes)
                  :default (empty-set)))))

(defun parse-category-definitions (glyphs)
  (parse-lines (parse-category-definition glyphs)
               (empty-map)
               (lambda (glyph glyphs)
                 (map-union glyphs glyph))))

(defun parse-category-section (glyphs)
  (parse-section "categories" (parse-category-definitions glyphs)))