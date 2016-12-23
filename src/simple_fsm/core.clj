(ns simple-fsm.core)
                                       
;; now if an event reaches the fsm you want it to transition
;; so you would want to say create-fsm(character, valid-transitions,
;; pre-action, action, post-action.
;; so the engine would process events for character while it has time
;; energy
   

(defmulti process-character (fn
                              [character event environ] [
                                                         (:character-type character)
                                                         (:state character)
                                                         (:event-type event)
                                                         ]
                              ))

(defn add-to-queue [agent event key]
  (conj (get-in agent [key]) event)
  )

(defn to-queue [key character event]
  (assoc-in character [key]
            (add-to-queue character event key)
            )
  )

(defn create-process-methods [character-type transition]
  (defmethod process-character [character-type (:old-state transition) (:event transition)]
    [character event environ]
    "run action on character, then return new character value"
    (if (< (:time-cost transition) (:time-energy character))
      (let [tp-char (assoc-in character [:time-energy]
                              (- (:time-energy character) (:time-cost transition)))]
            ( (:action transition) tp-char event environ)
            )
      (to-queue :deferred-event-queue character event ) ;; no TE so move to deferred    
    )
    )
  )

(defn take-character-energy
  [character cost]
  (assoc-in character [:time-energy]
            #(- %1 cost))
  )


(defn empty-queue
  [character queue-key]
  (assoc-in character [queue-key] [])
  )

(defn prep-character
  [character]
  (let [deferred (get-in character [:deferred-events])
        received (get-in character [:received-events])
        ]
    (let [requeued-character
          (assoc-in character [:event-queue]
                    (clojure.set/union deferred received))]
      (empty-queue
       (empty-queue requeued-character :received-events)
       :deferred-events)
      )
    )
  )
  
(defn integrate-character
  "integrate a character"  
  [character environ]
  (let [prepped-character (prep-character character)]
    (loop [[event & remaining] (:event-queue
                                prepped-character)
           prepped-character prepped-character]
      (let [processed-character
            (process-character prepped-character event environ)]
        (if (empty? remaining)
          (processed-character)
          (recur remaining processed-character)
          )
        )
      )
    )
  )


(defn send-event [origin destination event]
  "adds event to destination queue, but not before appending event with 
   a partial that allows destination to send response back"
  (let [enhanced-event (conj event {:send-response-fn
                                    (partial send-event destination origin)})]
    (send destination (partial to-queue :received-events) enhanced-event)                  
    )
  )
 
(defn split-fin-unfin
  [char-agents]
  { :not-done (filter #(empty? (:deferred-events @%1) ) char-agents)
   :done (filter #(not (empty? (:deferred-events @%1) )) char-agents) } 
  )

(defn add-time-energy
  [character delta]
  (assoc-in character [:time-energy]
            (+ (:time-energy character) delta ))
  )



;; the idea is first create a check to see if time-energy is positive
;; then if positive execute process-character, if not place back on queue (by
;; sending to self (using arguments from before)
(defn integrate-environ
  [environ characters time-delta]
  "so what you get with this  "
  ;; first all characters get energy delta added to them
  (let [character-plus-delta (map (partial add-time-energy time-delta) characters)]
    ;; now we turn the characters into agents, and store
    ;; their event queue next to them which is the current queue
    (loop [agent-characters
           (map agent character-plus-delta)
           processed []]
      ;; next we move their deferred into current queue
      (let [integrated-characters (map #(send integrate-character
                                                    %1 environ)
                                             agent-characters)]
        (map await integrated-characters)
        (let [fin-unfin (split-fin-unfin integrated-characters)]
          (if (empty? (:not-done fin-unfin))
            (:done fin-unfin)
            (recur (:not-done fin-unfin) (:done fin-unfin))
            )
          )))
    )
  )



(def event1 { :num-guest 1 :event-type :guest-arrived })

(def waiter1 (agent {:event-queue [] :name "waiter1"
                     :state :waiting :character-type :waiter :time-energy 5
                     :deferred-events []
                     :received-events [event1]}))
(def guest1 (agent {:event-queue [] :name "guest1"
                    :state :arrived :character-type :guest }))

(def partial-send (partial send-event guest1 waiter1)) 



;;(send waiter1 #(conj (:event-queue %) event1))


;;(send-event guest1 waiter1 event1)

;;(send waiter1 process-character event1 nil)


(def waiter-transition {:old-state :waiting :event :guest-arrived
                           :action (fn [character event environ]
                            (let [num-guests (:num-guest event)]
                              (println (str "seating " num-guests " guests!"))
                              )
                                     character 
                                     ) :new-state :seating-guests
                        :time-cost 2.0 })

(create-process-methods :waiter waiter-transition)

;;(integrate-character waiter1 [event1 event1] nil)

