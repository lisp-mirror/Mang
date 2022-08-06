(in-package #:mang)

(defun parse-glyph-definition (binary-features valued-features
                               privative-features
                               &optional
                                 (glyphs (empty-map (empty-map))))
  (declare (type set binary-features privative-features)
           (type map valued-features))
  (>>!
    name (parse-identifier (union *mang-reserved-symbols* (domain glyphs)))
    _ (>> (parse-whitespace)
          (parse-constant ":=")
          (parse-whitespace))
    phoneme (parse-feature-set binary-features valued-features
                               privative-features)
    (bind ((known (origin phoneme glyphs)))
      (if (empty? known)
          (succeed (map (name phoneme)
                        :default (empty-map)))
          (fail `(:duplicate-phoneme ,phoneme ,name ,known))))))

(defun parse-glyph-definitions (binary-features valued-features
                                privative-features
                                &optional
                                  (glyphs (empty-map (empty-map))))
  (declare (type set binary-features privative-features)
           (type map valued-features))
  (>>!
    _ (parse-whitespace)
    glyph (parse-glyph-definition binary-features valued-features
                                  privative-features glyphs)
    _ (parse-expression-end)
    (// (parse-glyph-definitions binary-features valued-features
                                 privative-features
                                 (map-union glyphs glyph))
        (succeed (map-union glyphs glyph)))))

(defun parse-glyph-section (binary-features valued-features privative-features)
  (declare (type set binary-features privative-features)
           (type map valued-features))
  (parse-section "glyphs"
                 (parse-glyph-definitions binary-features valued-features
                                          privative-features)))

(defun parse-glyph (glyphs)
  (// (<~ (parse-from-map glyphs)
          `(:no-glyph-found))
      (>>!
        candidate (parse-wrapped (parse-constant "<")
                                 (parse-identifier *mang-reserved-symbols*)
                                 (parse-constant ">"))
        ([av]if (@ glyphs candidate)
            (succeed `(,candidate ,it))
          (fail `(:unknown-glyph ,candidate))))))
