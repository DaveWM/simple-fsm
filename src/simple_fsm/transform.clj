(ns simple-fsm.transform
  (:require [clojure.set :refer :all]))

(defn add-to-queue [character event key]
  (conj (get-in character [key]) event)
  )

(defn to-queue [key character event]
  (assoc-in character [key]
            (add-to-queue character event key)
            )
  )

(defn take-character-energy
  [character cost]
  (assoc-in character [:time-energy]
            (- (:time-energy character) cost))
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

 
(defn split-fin-unfin
  "convert this to use vars/refs"
  [char-agents]
  { :done (filter #(empty? (:deferred-events @%1) ) char-agents)
   :not-done (filter #(not (empty? (:deferred-events @%1) )) char-agents) } 
  )

(defn fin-unfin-chute
  [character character-agent fin-unfin]
  (if (empty? (:deferred-events character))    
    (swap! (:done fin-unfin) conj character)
    (swap! (:not-done fin-unfin) conj character-agent)
    )
  character
  )
  
(defn split-fin-unfin2
  "so what you want to do is actually do is use send functions
   to send "
  [char-agents]
  (let [fin-unfin {:done (atom []) :not-done (atom [])}]
    (let [processed
          (map #(send %1 fin-unfin-chute %1 fin-unfin) char-agents)]
      (doall (map await processed)) ;; force side effect
      {:done @(:done fin-unfin) :not-done @(:not-done fin-unfin)}
      )
    )
  )
  

(defn add-time-energy
  [character delta]
  (assoc-in character [:time-energy]
            (+ (:time-energy character) delta ))
  )


(defn send-event [origin destination event]
  "adds event to destination queue, but not before appending event with 
   a partial that allows destination to send response back"
  (let [enhanced-event (conj event {:send-response-fn
                                    (partial send-event destination origin)})]
    (send destination (partial to-queue :received-events) enhanced-event)
    )
  )

(defn create-character
  [type name initial-state]
  { :name name :state initial-state :character-type type
   :event-queue []}
  )
