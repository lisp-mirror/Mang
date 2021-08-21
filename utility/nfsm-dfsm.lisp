(in-package #:mang)

;;;;; Definitions pertaining to nondeterministic finite state machines (NFSMs)
;;;;; and deterministic finite state machines (DFSMs). NFSMs should never be
;;;;; used anywhere but for creating DFSMs out of them. That is, outside this
;;;;; file, anything relating to NFSMs should be a method definition for the
;;;;; generic function NFSM<-.

(defun transition-closure (states epsilon-transition-map)
  "Compute the epsilon closure going out from STATES accoring to
EPSILON-TRANSITION-MAP"
  (labels ((_rec (states acc)
             (if (empty? states)
                 acc
                 (bind ((state (arb states))
                        (next (set-difference (@ epsilon-transition-map state)
                                              acc)))
                   (_rec (union (less states state)
                                next)
                         (with acc state))))))
    (_rec states (empty-set))))

(defun reachable-states (states input transition-map)
  (if (empty? states)
      (empty-set)
      (bind ((state (arb states)))
        (union (@ (@ transition-map state)
                  input)
               (reachable-states (less states state)
                                 input transition-map)))))

(defun epsilon-transition-map-with (epsilon-transition-map source target)
  (map* ($ epsilon-transition-map)
        :default (empty-set)
        (& source
           (set ($ it)
                target))))

(defun transition-table-with (transition-map source input target)
  (map* ($ transition-map)
        :default (empty-map (empty-set))
        (& (source transitions)
           (map* ($ transitions)
                 :default (empty-set)
                 (& (input targets)
                    (set ($ targets)
                         target))))))

;;;; The functions for generating NFSMs all return multiple values. Defining an
;;;; own NFSM class seems like overkill (since NFSMs shouldn't be used anywhere
;;;; else either way) and returning it in a list just adds additional overhead
;;;; (I assume).
;;;; The values returned are, in this order:
;;;; START-STATE ACCEPTING-STATE TRANSITION-TABLE EPSILON-TRANSITIONS
;;;; The start state and accepting state are just objects that are only used for
;;;; their identity.
;;;; The transition table does not contain the epsilon transitions. It is a map
;;;; of states to a map of possible transitions to a set of possible states
;;;; reached by taking the former transition when being in the former state.
;;;; The epsilon transitions are a map from states to sets of states reachable
;;;; without any transition from the former state.
(defmethod nfsm<- (obj)
  (bind ((in-state (gensym "in"))
         (out-state (gensym "out")))
    (values in-state out-state
            (map (in-state (map (obj (set out-state))
                                :default (empty-set)))
                 :default (empty-map (empty-set)))
            (empty-map (empty-set)))))

(defmethod nfsm<- ((set set))
  (bind ((in (gensym "in"))
         (out (gensym "out"))
         (transitions (empty-map (empty-map (empty-set))))
         (eps-transitions (empty-map (empty-set))))
    (do-set (inner set (values in out transitions eps-transitions))
      (multiple-value-bind (inner-in inner-out inner-transitions
                                     inner-eps-transitions)
          (nfsm<- inner)
        ([d]setf (map-union transitions inner-transitions
                            (lambda (x y)
                              (map-union x y #'union))))
        (setf eps-transitions
              (epsilon-transition-map-with
               (epsilon-transition-map-with
                (map-union eps-transitions inner-eps-transitions #'union)
                in inner-in)
               inner-out out))))
    (values in out transitions eps-transitions)))

(defmethod nfsm<- ((nothing null))
  (declare (ignore nothing))
  (bind ((state (gensym "state")))
    (values state state (empty-map (empty-map (empty-set)))
            (empty-map (empty-set)))))

(defun replace-state (original replacer transition-table eps-transitions)
  (values (image (lambda (source transitions)
                   (values (if (equal? source original)
                               replacer
                               source)
                           (image (lambda (transition targets)
                                    (values transition
                                            (image (lambda (target)
                                                     (if (equal? target
                                                                 original)
                                                         replacer
                                                         target))
                                                   targets)))
                                  transitions)))
                 transition-table)
          (image (lambda (source targets)
                   (values (if (equal? source original)
                               replacer
                               source)
                           (image (lambda (target)
                                    (if (equal? target original)
                                        replacer
                                        target))
                                  targets)))
                 eps-transitions)))

(defmethod nfsm<- ((cons cons))
  (bind ((car (car cons))
         (cdr (cdr cons)))
    (if (and car cdr)
        (bind (((:values car-in car-out car-transition-table
                         car-eps-transitions)
                (nfsm<- (car cons)))
               ((:values cdr-in cdr-out cdr-transition-table
                         cdr-eps-transitions)
                (nfsm<- (cdr cons)))
               ((:values cdr-transition-table cdr-eps-transitions)
                (replace-state cdr-in car-out cdr-transition-table
                               cdr-eps-transitions)))
          (values car-in cdr-out (map-union car-transition-table
                                            cdr-transition-table
                                            (lambda (x y)
                                              (map-union x y #'union)))
                  (map-union car-eps-transitions
                             cdr-eps-transitions
                             #'union)))
        (nfsm<- (or car cdr)))))


;;;; DFSM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dfsm-simplify-state-names (transition-map start-state accepting-states)
  (bind ((accepting (map<-set (independently (gensym "accept"))
                              accepting-states))
         (start (or (@ accepting start-state)
                    (gensym "start")))
         (registry (map (start-state start)
                        ($ accepting)))
         (new-dfsm (empty-map (empty-map nil))))
    (do-map (in transitions transition-map)
      (bind ((in-simple (or (@ registry in)
                            (gensym "state"))))
        ([d]setf (with registry in in-simple))
        (do-map (category target transitions)
          (bind ((target-simple (or (@ registry target)
                                    (gensym "state"))))
            ([d]setf (with registry target target-simple)
                     (with new-dfsm in-simple
                           (with (@ new-dfsm in-simple)
                                 category target-simple)))))))
    (values new-dfsm start (range accepting))))

(defun dfsm<-nfsm (start-state transition-map epsilon-transitions
                   accepting-state)
  (bind ((inputs (expand (lambda (state transitions)
                           (declare (ignore state))
                           (domain transitions))
                         transition-map)))
    (labels
        ((_rec (todo done acc)
           (if (empty? todo)
               acc
               (bind ((current (arb todo)))
                 (if (@ current done)
                     (_rec (less todo current)
                           done acc)
                     (let ((new (map<-set (lambda (input)
                                            (transition-closure
                                             (reachable-states current input
                                                               transition-map)
                                             epsilon-transitions))
                                          inputs)))
                       (_rec (less (union todo (set-difference (range new)
                                                               done))
                                   current)
                             (with done current)
                             (with acc current new))))))))
      (bind ((start-state (transition-closure (set start-state)
                                              epsilon-transitions))
             (dfsm (_rec (set start-state)
                         (empty-set)
                         (empty-map (empty-map (empty-set))))))
        (dfsm-simplify-state-names dfsm start-state
                                   (keep (lambda (state)
                                           (@ state accepting-state))
                                         (domain dfsm)))))))

(defun states<-transition-table (transition-table)
  (reduce (lambda (set map)
            (union (range map)
                   set))
          (range transition-table)
          :initial-value (domain transition-table)))

(defun collapse-states (transition-table accepting-states)
  (bind ((states (states<-transition-table transition-table))
         (valuable-states (empty-map))
         (redirections (empty-map))
         (new-transition-table (empty-map (empty-map nil))))
    (do-set (state states)
      (bind ((transitions (@ transition-table state))
             (identity (cons transitions (@ accepting-states state)))
             (found? (@ valuable-states identity))
             (new-state (or found? state)))
        (unless found?
          ([d]setf (with valuable-states
                         identity state)))
        ([d]setf (with redirections
                       state new-state)
                 (with new-transition-table
                       new-state transitions))))
    (image (lambda (state transitions)
             (values state
                     (map ($ (image (lambda (transition target)
                                      (values transition
                                              (@ redirections target)))
                                    transitions))
                          :default nil)))
           (filter (lambda (k v)
                     (declare (ignore k))
                     v)
                   new-transition-table))))

(defun collapse-states* (transition-table accepting-states)
  (bind ((collapsed (collapse-states transition-table accepting-states)))
    (if (equal? transition-table collapsed)
        collapsed
        (collapse-states* collapsed accepting-states))))

(defun prune-dfsm (transition-map start accepting)
  (declare (type map transition-map)
           (type symbol start)
           (type set accepting))
  (bind ((right-pruned (image (lambda (state transitions)
                                (values state
                                        (filter (lambda (transition target)
                                                  (declare (ignore transition))
                                                  (or (@ accepting target)
                                                      (not
                                                       (empty? (@ transition-map
                                                                  target)))))
                                                transitions)))
                              transition-map))
         (reachable-states (reduce (lambda (set map)
                                    (union (range map)
                                           set))
                                  (range right-pruned)
                                  :initial-value (empty-set))))
    (filter (lambda (k v)
              (or (eq k start)  ; preserve start state
                  ;; get rid of states that only lead back to themselves – we do
                  ;; not need to watch out for accepting states here, since all
                  ;; FSTs are required to not contain loops, so all loops can
                  ;; only exist in failure states
                  (and (not (empty? (less (range v)
                                          k)))
                       ;; get rid of states that are never reached
                       (@ reachable-states k))))
            right-pruned)))

(defun prune-dfsm* (transition-map start accepting)
  (bind ((pruned (prune-dfsm transition-map start accepting)))
    (if (equal? transition-map pruned)
        pruned
        (prune-dfsm* pruned start accepting))))

(defun dfsm-normalize-transitions (transitions start-state accepting-states)
  (prune-dfsm* (collapse-states* transitions accepting-states)
               start-state accepting-states))

(defclass dfsm ()
  ((%transitions :type map
                 :initarg :transitions
                 :initform (empty-map (empty-map (empty-set)))
                 :reader transitions<-)
   (%start-state :initarg :start-state
                 :reader start-state<-)
   (%accepting-states :type set
                      :initarg :accepting-states
                      :initform (empty-set)
                      :reader accepting-states<-)))

(defun dfsm-remove-infix-at (transition-table state infix)
  ;; BUG:
  ;;  If a state on the path of the infix is accepting, the newly created
  ;;  equivalent state is not going to be accepting. They should be, though.
  (if infix
      (chop curr infix
        (bind ((transitions (@ transition-table state))
               (next-state (@ transitions curr)))
          (if next-state
              (if infix
                  (bind ((next-transitions (@ transition-table next-state))
                         (reroute-state (gensym "state")))
                    (dfsm-remove-infix-at
                     (map ($ transition-table)
                          (state (map ($ transitions)
                                      (curr reroute-state)))
                          (reroute-state next-transitions))
                     reroute-state infix))
                  (map ($ transition-table)
                       (state (less transitions curr))))
              transition-table)))
      transition-table))

(defun dfsm-remove-infix (transition-table infix)
  (bind ((done (empty-set)))
    (loop
      :as state := (arb (set-difference (domain transition-table)
                                        done))
      :while state
      :do
         ([d]setf (dfsm-remove-infix-at transition-table state infix)
                  (with done state))
      :finally
         (return transition-table))))

(defmethod less ((collection dfsm)
                 (value1 list)
                 &optional value2)
  (declare (ignore value2))
  (bind ((transition-table (dfsm-remove-infix (transitions<- collection)
                                              value1))
         (accepting (intersection (states<-transition-table transition-table)
                                  (accepting-states<- collection)))
         (start (start-state<- collection))
         (transition-table (dfsm-normalize-transitions transition-table
                                                       start accepting)))
    (make-instance 'dfsm
                   :transitions transition-table
                   :start-state start
                   :accepting-states
                   (intersection (states<-transition-table transition-table)
                                 accepting))))

(defun dfsm-add-postfix-at (transition-table state postfix)
  (if postfix
      (chop curr postfix
        ([a]if (@ (@ transition-table state)
                  curr)
            (dfsm-add-postfix-at transition-table it postfix)
          (bind ((new-state (gensym "state")))
            (dfsm-add-postfix-at (map* ($ transition-table)
                                       :default (empty-map)
                                       (& (state transitions)
                                          (with transitions curr new-state)))
                                 new-state postfix))))
      (values transition-table state)))

(defmethod with ((collection dfsm)
                 (value1 list)
                 &optional value2)
  (declare (ignore value2))
  (bind ((start (start-state<- collection))
         ((:values transition-table new-accepting)
          (dfsm-add-postfix-at (transitions<- collection)
                               start value1))
         (accepting (with (accepting-states<- collection)
                          new-accepting))
         (transition-table (dfsm-normalize-transitions transition-table start
                                                       accepting)))
    (make-instance 'dfsm
                   :transitions transition-table
                   :start-state start
                   :accepting-states
                   (intersection (states<-transition-table transition-table)
                                 accepting))))

(defun dfsm<- (obj)
  (bind (((:values in out transitions eps-transitions)
          (nfsm<- obj))
         ((:values transitions in outs)
          (dfsm<-nfsm in transitions eps-transitions out))
         (transitions (filter (lambda (k v)
                                (declare (ignore k))
                                (not (empty? v)))
                              (dfsm-normalize-transitions transitions in
                                                          outs))))
    (make-instance 'dfsm
                   :transitions transitions
                   :start-state in
                   :accepting-states
                   (intersection (with (states<-transition-table transitions)
                                       in)
                                 outs))))

;;;; The following implementations for the matching via deterministic finite
;;;; state machine assume that there is a maximum length that the given DFSM can
;;;; match. This forbids matching for anything containing the * modificator, but
;;;; for matching reasonable words this is a perfectly reasonable
;;;; expectation. The DFSM builders implemented here definitely should only
;;;; produce DFSMs matching only constructs under a given length. If not, that
;;;; should be considered a bug.
(defmethod advance-dfsm ((dfsm dfsm)
                         (state symbol)
                         (word t))
  (@ (@ (transitions<- dfsm)
        state)
     word))

(defmethod advance-dfsm ((dfsm dfsm)
                         (state symbol)
                         (word list))
  (if (and state word)
      (advance-dfsm dfsm (@ (@ (transitions<- dfsm)
                               state)
                            (first word))
                    (rest word))
      state))

(defun run-dfsm (dfsm word)
  (bind ((result-state (advance-dfsm dfsm (start-state<- dfsm)
                                     word)))
    (values (@ (accepting-states<- dfsm)
               result-state)
            result-state)))


;;;; writing dot files for debugging purposes
(defmethod write-dot (stream (graph dfsm)
                      &key (node-label-key #'identity)
                        (edge-label-key #'identity))
  (format stream "digraph {~%")
  (bind ((accepting (accepting-states<- graph))
         (start (start-state<- graph)))
    (format stream (if (@ accepting start)
                       "  ~A [ shape=Mdiamond ]~%"
                       "  ~A [ shape=invtriangle ]~%")
            (funcall node-label-key (start-state<- graph)))
    (format stream "~{  ~A [ shape=diamond ]~%~}"
            (convert 'list
                     (image node-label-key (less accepting start)))))
  (do-map (source transitions (transitions<- graph))
    (do-map (transition target transitions)
      (format stream "  ~A -> ~A [ label = \"~A\" ]~%"
              (funcall node-label-key source)
              (funcall node-label-key target)
              (funcall edge-label-key transition))))
  (format stream "}~%"))
