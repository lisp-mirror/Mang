(in-package #:mang)

(defun parse-diachrony-body (binary-features valued-features privative-features
                             source-language
                             &key
                               (glyphs (glyphs<- source-language))
                               (categories (categories<- source-language)))
  (// (>>!
        _ (parse-section-header "replace")
        glyphs (parse-glyph-section binary-features valued-features
                                    privative-features)
        (parse-diachrony-body binary-features valued-features privative-features
                              source-language
                              :glyphs glyphs
                              :categories categories))
      (>>!
        _ (parse-section-header "replace")
        categories (parse-category-section (glyphs<- source-language))
        (parse-diachrony-body binary-features valued-features privative-features
                              source-language
                              :glyphs glyphs
                              :categories categories))
      (>>!
        _ (parse-section-header "augment")
        new-glyphs (parse-glyph-section binary-features valued-features
                                        privative-features)
        (parse-diachrony-body binary-features valued-features privative-features
                              source-language
                              :glyphs (map-union glyphs new-glyphs)
                              :categories categories))
      (>>!
        _ (parse-section-header "augment")
        new-categories (parse-category-section (glyphs<- source-language))
        (parse-diachrony-body binary-features valued-features privative-features
                              source-language
                              :glyphs glyphs
                              :categories (map-union categories
                                                     new-categories)))
      (>>!
        sound-changes (parse-sound-change-section binary-features
                                                  valued-features
                                                  privative-features
                                                  source-language)
        (parse-diachrony-body binary-features valued-features privative-features
                              (apply-sound-changes sound-changes source-language
                                                   :glyphs glyphs
                                                   :categories categories)))
      (>>!
        semantic-shifts (parse-semantic-shift-section source-language)
        (parse-diachrony-body binary-features valued-features privative-features
                              (apply-semantic-shifts semantic-shifts
                                                     source-language)))
      (succeed (copy-language source-language
                              :glyphs glyphs
                              :categories categories))))

(defun parse-diachrony-file (binary-features valued-features privative-features
                             source-language)
  (>>!
    _ (parse-section-header "diachrony")
    target-language (parse-diachrony-body binary-features valued-features
                                          privative-features source-language)
    _ (>> (parse-whitespace)
          (parse-eof))
    (succeed target-language)))
