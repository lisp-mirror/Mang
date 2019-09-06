(in-package #:mang)

(defun parse-language ()
  (>> (parse-whitespace)
      (parse-constant "##")
      (parse-whitespace-no-newline)
      (parse-identifier)))

(defun parse-mang ()
  (bind ((languages (empty-map (map (:glyphs (empty-map (empty-set)))
                                    (:categories (empty-map))
                                    (:generator (dfsm<- (empty-set)))
                                    (:markov-spec (empty-map))
                                    (:store (empty-map))
                                    (:dictionary (empty-map (empty-map))))))
         (language nil)
         (binary-features (empty-set))
         (valued-features (empty-map (empty-set)))
         (privative-features (empty-set))
         (glyphs (empty-map (empty-set)))
         (categories (empty-map))
         (generator (dfsm<- (empty-set)))
         (markov-spec (empty-map))
         (store (empty-map))
         (dictionary (empty-map (empty-map))))
    (many (parser-case
            (name
             (parse-language)
             (when language
               (setf languages
                     (with languages language
                           (map (:glyphs glyphs)
                                (:categories categories)
                                (:generator generator)
                                (:markov-spec markov-spec)
                                (:store store)
                                (:dictionary dictionary)))))
             (bind ((new-language (@ languages name)))
               (setf glyphs (@ new-language :glyphs)
                     categories (@ new-language :categories)
                     generator (@ new-language :generator)
                     markov-spec (@ new-language :markov-spec)
                     store (@ new-language :store)
                     dictionary (@ new-language :dictionary)
                     language name)))
            ((binary valued privative)
             (parse-feature-section)
             (setf binary-features binary
                   valued-features valued
                   privative-features privative))
            (new-glyphs
             (parse-glyph-section binary-features valued-features
                                  privative-features)
             (assert language)
             (setf glyphs (map-union glyphs new-glyphs)))
            (new-categories
             (parse-category-section glyphs)
             (assert language)
             (setf categories (map-union categories new-categories)))
            (new-generator
             (// (parse-wordgen-section glyphs categories)
                 (parse-clustergen-section glyphs categories))
             (assert language)
             (setf generator new-generator))
            ((new-markov-spec new-store)
             (parse-markov-section (range glyphs)
                                   categories)
             (assert language)
             (setf markov-spec new-markov-spec
                   store new-store))
            ((new-dictionary new-store)
             (parse-dictionary glyphs generator store markov-spec)
             (assert language)
             (setf dictionary (map-union dictionary new-dictionary #'map-union)
                   store new-store)))
          nil (lambda (a b)
                (declare (ignore a b))
                `(,binary-features ,valued-features ,privative-features
                                   ,languages)))))

(defun read-mang-files (file &rest files)
  (apply #'load-by-parser*
         (parse-mang)
         file files))
