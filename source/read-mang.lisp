(in-package #:mang)

(defun read-mang-files (feature-file language-file
                        &optional
                          dictionary-file)
  (bind (((:values binary-features valued-features privative-features)
          (load-feature-file feature-file))
         (language nil))
    (with-open-file (stream language-file)
      (bind (((:values loaded bus success?)
              (parser-call (parse-language-file binary-features valued-features
                                                privative-features)
                           stream)))
        (if success?
            (setf language loaded)
            (return-from read-mang-files
              `(:language-file-failure ,loaded ,bus)))))
    (if dictionary-file
        (with-open-file (stream dictionary-file)
          (values (parser-call (<~> (parse-dictionary-file language)
                                    (lambda (error)
                                      `(:dictionary-file-failure ,error)))
                               stream)))
        language)))
