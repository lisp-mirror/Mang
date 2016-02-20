(in-package #:mang-gui)

(defmacro defapp (name title position (width height)
                  (&rest local-defs)
                     (&rest options)
                  &body kids)
  `(defmodel ,name (gtk-app)
     (,@local-defs)
     (:default-initargs
         :title ,title
       :position ,position
       :width ,width :height ,height
       :kids (kids-list? ,@kids)
       ,@options)))

(defmacro +vbox ((&rest options)
                 &body kids)
  `(mk-vbox :kids (kids-list? ,@kids)
            ,@options))

(defmacro +hbox ((&rest options)
                 &body kids)
  `(mk-hbox :kids (kids-list? ,@kids)
            ,@options))

(defmacro +scrolled-window ((&rest options)
                            &body kids)
  `(mk-scrolled-window :kids (kids-list? ,@kids)
                       ,@options))

(defmacro +label (text &body options)
  `(mk-label :text ,text
             ,@options))

(defmacro +entry (name &body options)
  `(mk-entry :md-name ,name
             ,@options))

(defmacro +hscale (name value-type (min max init &optional (step 1))
                   &body options)
  `(mk-hscale :md-name ,name
              :value-type ',value-type
              :min ,min
              :max ,max
              :init ,init
              :step ,step
              ,@options))

(defmacro +spin-button (name (min max init &optional (step 1))
                        &body options)
  `(mk-spin-button :md-name ,name
                   :min ,min
                   :max ,max
                   :init ,init
                   :step ,step
                   ,@options))

(defmacro +button (label (w e d)
                   &body callback)
  `(mk-button :label ,label
              :on-clicked
              (callback (,w ,e ,d)
                ,@callback)))

(defmacro +listbox (name (&rest columns)
                            (&rest options)
                               items
                    &body print-fn)
  `(+scrolled-window ()
     (mk-listbox :md-name ,name
                 ,@(when columns
                     `(:columns (def-columns
                                  ,@(mapcar (lambda (column)
                                              `(:string (:title ,column)))
                                            columns))))
                 :print-fn (lambda (item)
                             ,@print-fn)
                 :items ,items
                 ,@options)))

(defmacro +treebox (name (&rest columns)
                            (&rest options)
                               roots
                               (&body print-fn)
                    &body children-fn)
  `(+scrolled-window ()
     (mk-treebox :md-name ,name
                 ,@(when columns
                         `(:columns (def-columns
                                      ,@(mapcar (lambda (column)
                                                  `(:string (:title ,column)))
                                                columns))))
                 :print-fn (lambda (item)
                             ,@print-fn)
                 :roots ,roots
                 :children-fn (lambda (item)
                                ,@children-fn)
                 ,@options
                 :selection-mode :single)))

(defmacro +notebook ((&rest options)
                     &body tabs &key &allow-other-keys)
  (let ((tab-labels (loop for label in tabs by #'cddr
                       collect label))
        (tab-contents (loop for content in (cdr tabs)
                         by #'cddr
                         collect content)))
    `(mk-notebook :tab-labels ',tab-labels
                  :kids (kids-list? ,@tab-contents))))

(defmacro app-value (name)
  `(,name (upper self gtk-app)))
