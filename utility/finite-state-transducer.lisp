(in-package #:mang)

(defclass fst ()
  ((%transitions :type map
                 :initarg :transitions
                 :reader transitions<-)
   (%preferred :type map
               :initarg :preferred
               :reader preferred<-)
   (%start-state :type symbol
                 :initarg :start-state
                 :reader start-state<-)
   (%accepting-states :type set
                      :initarg :accepting-states
                      :reader accepting-states<-)))

(defun fst (transitions &key
                          (preferred (empty-map (empty-map (empty-set))))
                          (start-state (gensym "start"))
                          (accepting-states (set (gensym "accepting"))))
  (declare (type map transitions))
  (make-instance 'fst
                 :transitions transitions
                 :preferred preferred
                 :start-state start-state
                 :accepting-states accepting-states))

(defun empty-fst? (fst)
  (and (empty? (transitions<- fst))
       (empty? (preferred<- fst))))

(defun modify-fst (fst &key
                         transitions preferred start-state accepting-states
                         new-transitions new-preferred new-accepting-states)
  (fst (if new-transitions
           (map-union (transitions<- fst)
                      new-transitions
                      (lambda (t1 t2)
                        (map-union t1 t2
                                   #'union)))
           (or transitions (transitions<- fst)))
       :preferred
       (if new-preferred
           (map-union (preferred<- fst)
                      new-preferred
                      (lambda (t1 t2)
                        (map-union t1 t2
                                   #'union)))
           (or preferred (preferred<- fst)))
       :start-state
       (or start-state (start-state<- fst))
       :accepting-states
       (if new-accepting-states
           (union (accepting-states<- fst)
                  new-accepting-states)
           (or accepting-states (accepting-states<- fst)))))

(defun add-epsilon-transition (fst state1 state2 &optional out)
  (bind ((out (if (functionp out)
                  out
                  (constantly out))))
    (modify-fst fst
                :new-transitions
                (map (state1 (map (#'true (list out state2 nil))
                                  :default (empty-set)))
                     :default (empty-map (empty-set))))))

(defun add-epsilon-transitions (fst &rest transitions &key &allow-other-keys)
  (declare (type fst fst))
  (bind ((fst fst))
    (loop :for (source target)
       :on transitions
       :do (setf fst (add-epsilon-transition fst source target))
       :finally (return fst))))

(defun empty-fst (&optional (state (gensym "state")))
  (fst (empty-map (empty-map (empty-set)))
       :start-state state
       :accepting-states (set state)))

(defun fst-elementary (condition out &key (in-state (gensym "elementary-in"))
                                       (out-state (gensym "elementary-out"))
                                       (consume? t))
  (bind ((out (if (functionp out)
                  out
                  (constantly (ensure-list out))))
         (condition (if (functionp condition)
                        condition
                        (lambda (x)
                          (equal? x condition)))))
    (fst (map (in-state (map (condition (set (list out out-state consume?)))
                             :default (empty-set)))
              :default (empty-map (empty-set)))
         :start-state in-state
         :accepting-states (set out-state))))

(defun fst-consume ()
  (fst-elementary #'true '()
                  :consume? t))

(defun fst-sequence (fst1 fst2 &key (in-state (gensym "sequence-in"))
                                 (out-state (gensym "sequence-out")))
  (modify-fst fst1
              :start-state in-state
              :accepting-states (set out-state)
              :new-transitions
              (map ($ (transitions<- fst2))
                   ($
                    (map<-set (constantly (map (#'true
                                                (set (list (constantly nil)
                                                           (start-state<- fst2)
                                                           nil)))
                                               :default (empty-set)))
                              (accepting-states<- fst1)
                              (empty-set)))
                   ($ (map<-set (constantly (map (#'true
                                                  (set (list (constantly nil)
                                                             out-state
                                                             nil)))
                                                 :default (empty-set)))
                                (accepting-states<- fst2)
                                (empty-set)))
                   (in-state (map (#'true (set (list (constantly nil)
                                                     (start-state<- fst1)
                                                     nil)))
                                  :default (empty-set)))
                   :default (empty-map (empty-set)))
              :new-preferred (preferred<- fst2)))

(defun fst-sequence* (&rest fsts)
  (if fsts
      (destructuring-bind (current &rest rest)
          fsts
        (fst-sequence current (apply #'fst-sequence*
                                     rest)))
      (empty-fst)))

(defun fst-alternate (fst1 fst2 &key (in-state (gensym "alternate-in"))
                                  (out-state (gensym "alternate-out")))
  (modify-fst fst1
              :start-state in-state
              :accepting-states (set out-state)
              :new-transitions
              (map ($ (transitions<- fst2))
                   (in-state (map (#'true (set (list (constantly nil)
                                                     (start-state<- fst1)
                                                     nil)
                                               (list (constantly nil)
                                                     (start-state<- fst2)
                                                     nil)))
                                  :default (empty-set)))
                   ($ (map<-set (constantly (map (#'true
                                                  (set (list (constantly nil)
                                                             out-state
                                                             nil)))
                                                 :default (empty-set)))
                                (union (accepting-states<- fst1)
                                       (accepting-states<- fst2))
                                (empty-set)))
                   :default (empty-map (empty-set)))
              :new-preferred (preferred<- fst2)))

(defun fst-preferred (preferred fallback &key (in-state (gensym "preferred-in"))
                                           (out-state (gensym "preferred-out")))
  (modify-fst preferred
              :start-state in-state
              :accepting-states (set out-state)
              :new-transitions
              (map ($ (transitions<- fallback))
                   (in-state (map (#'true (set (list (constantly nil)
                                                     (start-state<- fallback)
                                                     nil)))
                                  :default (empty-set)))
                   ($ (map<-set (constantly (map (#'true
                                                  (set (list (constantly nil)
                                                             out-state
                                                             nil)))
                                                 :default (empty-set)))
                                (union (accepting-states<- preferred)
                                       (accepting-states<- fallback))
                                (empty-set)))
                   :default (empty-map (empty-set)))
              :new-preferred
              (map ($ (preferred<- fallback))
                   (in-state (map (#'true (set (list (constantly nil)
                                                     (start-state<- preferred)
                                                     nil)))
                                  :default (empty-set)))
                   :default (empty-map (empty-set)))))

(defun fst-maybe (fst &key (in-state (gensym "maybe-in"))
                        (out-state (gensym "maybe-out")))
  (bind ((maybe-before (gensym "maybe-before"))
         (maybe-after (gensym "maybe-after")))
    (modify-fst fst
                :start-state in-state
                :accepting-states (set out-state)
                :new-transitions
                (map (in-state (map (#'true (set (list (constantly nil)
                                                       maybe-before nil)))
                                    :default (empty-set)))
                     (maybe-before (map (#'true (set (list (constantly nil)
                                                           (start-state<- fst)
                                                           nil)
                                                     (list (constantly nil)
                                                           maybe-after
                                                           nil)))
                                        :default (empty-set)))
                     ($ (map<-set (constantly (map (#'true
                                                    (set (list (constantly nil)
                                                               maybe-after
                                                               nil)))
                                                   :default (empty-set)))
                                  (accepting-states<- fst)
                                  (empty-set)))
                     (maybe-after (map (#'true (set (list (constantly nil)
                                                          out-state nil)))
                                       :default (empty-set)))
                     :default (empty-map (empty-set))))))

(defun fst-repeat (fst &key (in-state (gensym "repeat-in"))
                         (out-state (gensym "repeat-out")))
  (bind ((repeat-before (gensym "repeat-before"))
         (repeat-after (gensym "repeat-after")))
    (modify-fst fst
                :start-state in-state
                :accepting-states (set out-state)
                :new-transitions
                (map (in-state (map (#'true (set (list (constantly nil)
                                                       repeat-before nil)))
                                    :default (empty-set)))
                     (repeat-before (map (#'true (set (list (constantly nil)
                                                            (start-state<- fst)
                                                            nil)))
                                         :default (empty-set)))
                     ($ (map<-set (constantly (map (#'true
                                                    (set (list (constantly nil)
                                                               repeat-after
                                                               nil)))
                                                   :default (empty-set)))
                                  (accepting-states<- fst)
                                  (empty-set)))
                     (repeat-after (map (#'true (set (list (constantly nil)
                                                           repeat-before
                                                           nil)
                                                     (list (constantly nil)
                                                           out-state nil)))
                                        :default (empty-set)))
                     :default (empty-map (empty-set))))))

(defun fst-preferred-transitions (fst state glyph not-end?)
  (declare (type fst fst)
           (type symbol state))
  (reduce #'union
          (range (if not-end?
                     (all-fitting (@ (preferred<- fst)
                                     state)
                                  glyph)
                     (image (lambda (key value)
                              (values key
                                      (filter (lambda (value)
                                                (and (eq key #'true)
                                                     (not (third value))))
                                              value)))
                            (@ (preferred<- fst)
                               state))))
          :initial-value (empty-set)))

(defun fst-applicable-transitions (fst state glyph not-end?)
  (declare (type fst fst)
           (type symbol state))
  (reduce #'union
          (range (if not-end?
                     (all-fitting (@ (transitions<- fst)
                                     state)
                                  glyph)
                     (image (lambda (key value)
                              (values key
                                      (filter (lambda (value)
                                                (and (eq key #'true)
                                                     (not (third value))))
                                              value)))
                            (@ (transitions<- fst)
                               state))))
          :initial-value (empty-set)))

(defun fst-solutions (fst state input)
  (declare (type fst fst)
           (type symbol state)
           (type (or cons null)
                 input))
  (bind ((glyph (first input)))
    (labels ((_expand (transitions)
               (if (empty? transitions)
                   (if (and (empty? input)
                            (@ (accepting-states<- fst)
                               state))
                       (set '())
                       (empty-set))
                   (cross-product
                    #'append
                    (image (lambda (transition)
                             (funcall (first transition)
                                      glyph))
                           transitions)
                    (reduce #'union
                            (image (lambda (transition)
                                     (fst-solutions fst (second transition)
                                                    (if (third transition)
                                                        (rest input)
                                                        input)))
                                   transitions)
                            :initial-value (empty-set))))))
      ([d]if (empty?
              (_expand (fst-preferred-transitions fst state glyph input)))
          (_expand (fst-applicable-transitions fst state glyph input))
        it))))

(defun run-fst (fst input)
  (fst-solutions fst (start-state<- fst)
                 input))
