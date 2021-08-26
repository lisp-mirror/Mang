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
                              (copy-language source-language
                                             :new-glyphs new-glyphs)))
      (>>!
        _ (parse-section-header "augment")
        new-categories (parse-category-section (glyphs<- source-language))
        (parse-diachrony-body binary-features valued-features privative-features
                              (copy-language source-language
                                             :new-categories new-categories)))
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
    _ (>> (parse-section-header "diachrony")
          (parse-whitespace))
    target-language (parse-diachrony-body binary-features valued-features
                                          privative-features source-language)
    _ (>> (parse-whitespace)
          (parse-eof))
    (succeed target-language)))

(defun load-diachrony-file (file binary-features valued-features
                            privative-features source-language)
  (with-file-bus (bus file)
    (parser-call (parse-diachrony-file binary-features valued-features
                                       privative-features source-language)
                 bus)))
