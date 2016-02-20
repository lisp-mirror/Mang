(in-package #:mang-gui)

(defun parse-pattern (string)
  (declare (type (or string null)
                 string))
  (the (or [pattern] null)
       (when string
         (if (string= string "")
             <end>
             (let ((mode (elt string 0)))
               (case mode
                 (#\#
                  (let ((pattern-tail (parse-pattern (subseq string 1))))
                    (when pattern-tail
                      (<this> pattern-tail))))
                 (#\_
                  (let ((pattern-tail (parse-pattern (subseq string 1))))
                    (when pattern-tail
                      (<any> pattern-tail))))
                 (#\|
                  (let ((glyph-end (1+ (position #\| (subseq string 1)))))
                    (when glyph-end
                      (let ((glyph (subseq string 1 glyph-end))
                            (pattern-tail (parse-pattern (subseq string (1+ glyph-end)))))
                        (when pattern-tail
                          (<exactly> (<glyph> glyph)
                                     pattern-tail))))))))))))

(defmatch display ([pattern])
    string
  (<end>
   "")
  ((<this> p)
   (concatenate 'string
                "#" (display p)))
  ((<any> p)
   (concatenate 'string
                "_" (display p)))
  ((<exactly> (<glyph> g)
              p)
   (concatenate 'string
                "|" g "|" (display p))))

(defmd pattern-box (hbox)
  :kids
  (kids-list?
   (+vbox (:expand t :fill t)
     (+hbox ()
       (+vbox ()
         (+label "Category:")
         (+entry :new-category))
       (+button
           (c?
             (let ((new-category (widget-value :new-category)))
               (if new-category
                   (format nil "Add category '~A'"
                           new-category)
                   "No category to add")))
           (w e d)
         (let ((new-category (widget-value :new-category)))
           (when new-category
             (setf (app-value categories)
                   (with (app-value categories)
                         new-category))))))
     (+hbox ()
       (+vbox ()
         (+label "Pattern:")
         (+entry :new-pattern))
       (+vbox ()
         (+label "Weight:")
         (+spin-button :pattern-weight (1 1000 1)))
       (+button
           (c?
             (let* ((new-pattern (widget-value :new-pattern))
                    (pattern-weight (widget-value :pattern-weight))
                    (category (widget-value :pattern-categories)))
               (if (parse-pattern new-pattern)
                   (if category
                       (if pattern-weight
                           (format nil "Add pattern '~A' to category ~A with weight ~A"
                                   new-pattern category pattern-weight)
                           (format nil "Please enter a valid weight for pattern ~A in category ~A"
                                   new-pattern category))
                       (format nil "Please choose a category to add the pattern '~A' to in the category list below"
                               new-pattern))
                   "No valid pattern to add")))
           (w e d)
         (let* ((new-pattern (parse-pattern (widget-value :new-pattern)))
                (category (widget-value :pattern-categories))
                (weight (widget-value :pattern-weight)))
           (when (and new-pattern category weight)
             (setf (app-value learn-patterns)
                   (let ((lcs (app-value learn-patterns)))
                     (with lcs category
                           (with (lookup lcs category)
                                 new-pattern weight))))))))
     (+button
         (c? (let ((new-category (widget-value :new-category))
                   (category (widget-value :pattern-categories)))
               (if (and new-category category (not (string= category new-category)))
                   (format nil "Copy category '~A' as category '~A'"
                           category new-category)
                   "Not enough information to copy category")))
         (w e d)
       (let ((new-category (widget-value :new-category))
             (category (widget-value :pattern-categories))
             (patterns (app-value learn-patterns)))
         (when (and new-category category (not (string= new-category category)))
           (setf (app-value categories)
                 (with app-value categories new-category))
           (setf (app-value learn-patterns)
                 (with patterns new-category
                       (lookup learn-patterns category))))))
     (+hbox (:expand t :fill t)
       (+listbox :pattern-categories ("Category")
           ()
           (c? (convert 'list
                        (app-value categories)))
         (list item))
       (+listbox :patterns ("Pattern" "Weight")
           ()
           (c? (convert 'list
                        (or (lookup (app-value learn-patterns)
                                    (widget-value :pattern-categories))
                            (empty-map))))
         (list (display (car item))
               (format nil "~D"
                       (cdr item))))))))
