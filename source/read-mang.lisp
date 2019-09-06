(in-package #:mang)

(defun parse-language ()
  (>> (parse-whitespace)
      (parse-constant "##")
      (parse-whitespace-no-newline)
      (parse-identifier)))

(defun parse-mang (&optional
                     (languages (empty-map (map (:glyphs
                                                 (empty-map (empty-set)))
                                                (:categories
                                                 (empty-map))
                                                (:generator
                                                 (dfsm<- (empty-set)))
                                                (:markov-spec
                                                 (empty-map
                                                  (empty-map
                                                   (constantly <nodist>))))
                                                (:store
                                                 (empty-map
                                                  (constantly <nodist>)))
                                                (:dictionary
                                                 (empty-map (empty-map))))))
                     (language nil)
                     (binary-features (empty-set))
                     (valued-features (empty-map (empty-set)))
                     (privative-features (empty-set))
                     (glyphs (empty-map (empty-set)))
                     (categories (empty-map))
                     (generator (dfsm<- (empty-set)))
                     (markov-spec (empty-map (empty-map (constantly <nodist>))))
                     (store (empty-map (empty-map <nodist>)))
                     (dictionary (empty-map (empty-map))))
  (>>!
    (languages language binary-features valued-features privative-features
               glyphs categories generator markov-spec store dictionary)
    (// (<$!> name (parse-language)
          (bind ((new-language (@ languages name)))
            `(,(if language
                   (with languages language
                         (map (:glyphs glyphs)
                              (:categories categories)
                              (:generator generator)
                              (:markov-spec markov-spec)
                              (:store store)
                              (:dictionary dictionary)))
                   languages)
               ,name ,binary-features ,valued-features ,privative-features
               ,(@ new-language :glyphs)
               ,(@ new-language :categories)
               ,(@ new-language :generator)
               ,(@ new-language :markov-spec)
               ,(@ new-language :store)
               ,(@ new-language :dictionary))))
        (<$!> (binary-features valued-features privative-features)
            (parse-feature-section)
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories ,generator
                       ,markov-spec ,store ,dictionary))
        (<$!> glyphs (parse-glyph-section binary-features valued-features
                                          privative-features)
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories ,generator
                       ,markov-spec ,store ,dictionary))
        (<$!> categories (parse-category-section glyphs)
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories ,generator
                       ,markov-spec ,store ,dictionary))
        (<$!> generator (// (parse-wordgen-section glyphs categories)
                            (parse-clustergen-section glyphs categories))
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories ,generator
                       ,markov-spec ,store ,dictionary))
        (<$!> (markov-spec store)
            (parse-markov-section (range glyphs)
                                  categories)
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories ,generator
                       ,markov-spec ,store ,dictionary))
        (<$!> (dictionary store)
            (parse-dictionary glyphs generator store markov-spec)
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories ,generator
                       ,markov-spec ,store ,dictionary)))
    (languages _ binary-features valued-features privative-features _ _ _ _ _ _)
    (parse-mang languages language binary-features valued-features
                privative-features glyphs categories generator markov-spec store
                dictionary)
    (succeed `(,binary-features ,valued-features ,privative-features
                                ,languages))))

(defun read-mang-files (file &rest files)
  (apply #'load-by-parser*
         (parse-mang)
         file files))
