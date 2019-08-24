(in-package #:mang)

(defun parse-glyph-definition (binary-features valued-features
                               privative-features)
  (declare (type set binary-features privative-features)
           (type map valued-features))
  (>>!
    name (parse-identifier *mang-reserved-symbols*)
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    phoneme (parse-feature-set binary-features valued-features
                               privative-features)
    (succeed (map (name phoneme)
                  :default (empty-map)))))

(defun parse-glyph-definitions (binary-features valued-features
                                privative-features)
  (declare (type set binary-features privative-features)
           (type map valued-features))
  (parse-lines (parse-glyph-definition binary-features valued-features
                                       privative-features)
               (empty-map (empty-set))
               (lambda (glyph glyphs)
                 (map-union glyphs glyph))))

(defun parse-glyph-section (binary-features valued-features privative-features)
  (declare (type set binary-features privative-features)
           (type map valued-features))
  (parse-section "glyphs"
                 (parse-glyph-definitions binary-features valued-features
                                          privative-features)))

(defun parse-glyph (glyphs)
  (>>!
    candidate (// (parse-anything)
                  (parse-wrapped "<" (parse-identifier *mang-reserved-symbols*)
                                 ">"))
    ([av]if (@ glyphs candidate)
        (succeed `(,candidate ,it))
      (fail `(:unknown-glyph ,candidate)))))
