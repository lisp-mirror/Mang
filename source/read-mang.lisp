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
                                                  (empty-map
                                                   (empty-map <nodist>))))
                                                (:dictionary
                                                 (empty-map (empty-map))))))
                     (language nil)
                     (binary-features (empty-set))
                     (valued-features (empty-map (empty-set)))
                     (privative-features (empty-set))
                     (glyphs (empty-map (empty-set)))
                     (categories (empty-map))
                     (sonority-hierarchy '())
                     (generator (dfsm<- (empty-set)))
                     (markov-spec (empty-map (empty-map (constantly <nodist>))))
                     (store (empty-map (empty-map <nodist>)))
                     (dictionary (empty-map (empty-map))))
  (>>!
    (languages language binary-features valued-features privative-features
               glyphs categories sonority-hierarchy generator markov-spec store
               dictionary)
    (// (<$!> name (parse-language)
          (bind ((languages (if language
                                (with languages language
                                      (map (:glyphs glyphs)
                                           (:categories categories)
                                           (:sonority-hierarchy
                                            sonority-hierarchy)
                                           (:generator generator)
                                           (:markov-spec markov-spec)
                                           (:store store)
                                           (:dictionary dictionary)))
                                languages))
                 (new-language (@ languages name)))
            `(,languages ,name ,binary-features ,valued-features
                         ,privative-features
                         ,(@ new-language :glyphs)
                         ,(@ new-language :categories)
                         ,(@ new-language :sonority-hierarchy)
                         ,(@ new-language :generator)
                         ,(@ new-language :markov-spec)
                         ,(@ new-language :store)
                         ,(@ new-language :dictionary))))
        (<$!> (binary-features valued-features privative-features)
            (parse-feature-section)
          (print "parsing features")
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> glyphs (parse-glyph-section binary-features valued-features
                                          privative-features)
          (print "parsing glyphs")
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> categories (parse-category-section glyphs)
          (print "parsing categories")
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> sonority-hierarchy (parse-sonority-hierarchy glyphs categories)
          (print "parsing sonority hierarchy")
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> generator (// (parse-wordgen-section glyphs categories)
                            (parse-clustergen-section glyphs categories))
          (print "parsing generators")
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> (markov-spec store)
            (parse-markov-section glyphs categories)
          (print "parsing markov specs")
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> (dictionary store)
            (parse-dictionary glyphs generator store markov-spec)
          (print "parsing dictionary")
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary)))
    (languages language binary-features valued-features privative-features
               glyphs categories sonority-hierarchy generator markov-spec store
               dictionary)
    (<? (parse-mang languages language binary-features valued-features
                    privative-features glyphs categories sonority-hierarchy
                    generator markov-spec store dictionary)
        `(,languages ,language ,binary-features ,valued-features
                     ,privative-features ,glyphs ,categories ,sonority-hierarchy
                     ,generator ,markov-spec ,store ,dictionary))
    (succeed `(,(if language
                    (with languages language
                          (map (:glyphs glyphs)
                               (:categories categories)
                               (:sonority-hierarchy sonority-hierarchy)
                               (:generator generator)
                               (:markov-spec markov-spec)
                               (:store store)
                               (:dictionary dictionary)))
                    languages)
               ,language ,binary-features ,valued-features ,privative-features
               ,glyphs ,categories ,sonority-hierarchy ,generator ,markov-spec
               ,store ,dictionary))))

(defun read-mang-files (file &rest files)
  (bind (((:values r _ ?)
          (apply #'load-by-parser*
                 (parse-mang)
                 file files)))
    (if ?
        (bind (((languages _ binary-features valued-features privative-features
                           &rest _)
                r))
          (values binary-features valued-features privative-features languages
                  nil t))
        (values (empty-set)
                (empty-map (empty-set))
                (empty-set)
                (empty-map (map (:glyphs (empty-map (empty-set)))
                                (:categories (empty-map))
                                (:generator (dfsm<- (empty-set)))
                                (:markov-spec (empty-map
                                               (empty-map
                                                (constantly <nodist>))))
                                (:store (empty-map (empty-map <nodist>)))
                                (:dictionary (empty-map (empty-map)))))
                r
                nil))))
