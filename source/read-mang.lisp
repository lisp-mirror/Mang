(in-package #:mang)

(defun read-mang-files (feature-file language-file
                        &key
                          dictionary-file
                          diachrony-file)
  (bind (((:values binary-features valued-features privative-features)
          (load-feature-file feature-file))
         (language nil))
    (with-open-file (stream language-file)
      (values (parser-call (<$~> (parse-language-file binary-features
                                                      valued-features
                                                      privative-features)
                                 (lambda (l)
                                   (setf language l))
                                 (lambda (error)
                                   (return-from read-mang-files
                                     `(:language-file-failure ,error))))
                           stream)))
    (when dictionary-file
      (with-open-file (stream dictionary-file)
        (values (parser-call (<$~> (parse-dictionary-file language)
                                   (lambda (l)
                                     (setf language l))
                                   (lambda (error)
                                     (return-from read-mang-files
                                       `(:dictionary-file-failure ,error))))
                             stream))))
    (when diachrony-file
      (with-open-file (stream diachrony-file)
        (values (parser-call (<$~> (parse-diachrony-file binary-features
                                                         valued-features
                                                         privative-features
                                                         language)
                                   (lambda (l)
                                     (setf language l))
                                   (lambda (error)
                                     (return-from read-mang-files
                                       `(:diachrony-file-error ,error))))
                             stream))))
    language))
