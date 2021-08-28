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

(defun simplest-mang (feature-file language-file dictionary-file
                      &key
                        diachrony-file
                        computer-readable?)
  (bind ((language (read-mang-files feature-file language-file
                                    :dictionary-file dictionary-file
                                    :diachrony-file diachrony-file)))
    (if (listp language)
        (bind (((error-type error)
                language))
          (format *error-output* (ecase error-type
                                   (:language-file-failure
                                    "Language file failed to load: ~A")
                                   (:dictionary-file-failure
                                    "Dictionary file failed to load: ~A")
                                   (:diachrony-file-error
                                    "Diachrony file failed to load: ~A"))
                  error))
        (loop
          :while (ask-arbitrary-known-gloss! language)
          :finally
             (write-dictionary t language
                               :computer-readable? computer-readable?)))))
