(in-package #:mang)

(defun parse-mang (interactive? &optional
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
    (// (<$!> name (parse-language-header)
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
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> glyphs (parse-glyph-section binary-features valued-features
                                          privative-features)
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> categories (parse-category-section glyphs)
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> sonority-hierarchy (parse-sonority-hierarchy glyphs categories)
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> generator (// (parse-wordgen-section glyphs categories)
                            (parse-clustergen-section glyphs categories))
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> (markov-spec store)
            (parse-markov-section glyphs categories)
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary))
        (<$!> (dictionary store)
            (parse-dictionary glyphs generator store markov-spec interactive?)
          `(,languages ,language ,binary-features ,valued-features
                       ,privative-features ,glyphs ,categories
                       ,sonority-hierarchy ,generator ,markov-spec ,store
                       ,dictionary)))
    (languages language binary-features valued-features privative-features
               glyphs categories sonority-hierarchy generator markov-spec store
               dictionary)
    (<? (parse-mang interactive?
                    languages language binary-features valued-features
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

(defun read-mang-files (interactive? file &rest files)
  (bind (((:values r _ ?)
          (apply #'load-by-parser*
                 (parse-mang interactive?)
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

(defmacro load-mang-files! (interactive? (file &rest files)
                            binary-features valued-features privative-features
                            &body languages)
  (bind ((g!binary-features (gensym "binary-features"))
         (g!valued-features (gensym "valued-features"))
         (g!privative-features (gensym "privative-features"))
         (g!languages (gensym "languages"))
         (o!name (gensym "name"))
         (g!language (gensym "language")))
    `(bind (((:values ,g!binary-features ,g!valued-features
                      ,g!privative-features ,g!languages)
             (read-mang-files ,interactive? ,file ,@files)))
       (setf ,binary-features ,g!binary-features
             ,valued-features ,g!valued-features
             ,privative-features ,g!privative-features)
       ,@(loop
           :for (name glyphs categories generator markov-spec store dictionary)
             :in languages
           :collect
           `(bind ((,o!name ,name)
                   (,g!language (@ ,g!languages ,o!name)))
              (setf ,glyphs (@ ,g!language :glyphs)
                    ,categories (@ ,g!language :categories)
                    ,generator (@ ,g!language :generator)
                    ,markov-spec (@ ,g!language :markov-spec)
                    ,store (@ ,g!language :store)
                    ,dictionary (@ ,g!language :dictionary))))
       t)))

(defun run-mang (interactive? computer-readable? output file &rest files)
  (bind ((languages (nth-value 3 (apply #'read-mang-files
                                        interactive? file files))))
    (with-open-file (stream output
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (image (lambda (name language)
               (format stream "## ~A~%"
                       name)
               (write-dictionary stream (@ language :glyphs)
                                 (@ language :dictionary)
                                 :computer-readable? computer-readable?))
             languages))
    t))

(defun mang ()
  (bind (((options files output)
          (split-sequence "-" (rest (unix-opts:argv))
                          :test #'string=)))
    (apply #'run-mang
           (or ([a]and (position "-c" options
                                 :test #'string=)
                       (parser-call (parse-number)
                                    (nth (1+ it)
                                         options)))
               (not (not (find "-i" options
                               :test #'string=))))
           (not (not (find "-r" options
                           :test #'string=)))
           (first output)
           files)))
