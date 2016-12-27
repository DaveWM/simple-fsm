(ns simple-fsm.fsm
  (:require [simple-fsm.transform :as transform]))


(defmulti process-character
  (fn
    [character event environ]
    [
     (:character-type character)
     (:state character)
     (:event-type event)
     ]
    ))

(defn create-process-methods [character-type transitions]
  (loop [[transition & rest] transitions]
    (println "creating dispatch: "
             [character-type (:old-state transition) (:event transition)])    
    
    (defmethod process-character
      [character-type (:old-state transition) (:event transition)]
      [character event environ]
      "run action on character, then return new character value"
      (if (<= (:time-cost transition) (:time-energy character))
        (let [tp-char (assoc-in character [:time-energy]
                                (- (:time-energy character)
                                   (:time-cost transition)))]
          (assoc-in
           ( (:action transition) tp-char event environ) 
           [:state] (:new-state transition))
          )        
        (transform/to-queue
         :deferred-event-queue character event ) ;; no TE so move to deferred
        )
      )
    (if (empty? rest)
      nil
      (recur rest)
      )
    )
  )

(defn integrate-character
  "integrate a character"  
  [character environ]
  (let [prepped-character (transform/prep-character character)]
    (loop [[event & remaining] (:event-queue
                                prepped-character)
           prepped-character prepped-character]
      (let [processed-character
            (process-character prepped-character event environ)]
        (if (empty? remaining)
          processed-character
          (recur remaining processed-character)
          )
        )
      )
    )
  )


;; the idea is first create a check to see if time-energy is positive
;; then if positive execute process-character, if not place back on queue (by
;; sending to self (using arguments from before)
(defn integrate-environ
  [environ characters time-delta]
  "so what you get with this"
  ;; first all characters get energy delta added to them
  (let [character-plus-delta (map (partial transform/add-time-energy time-delta) characters)]
    ;; now we turn the characters into agents, and store
    ;; their event queue next to them which is the current queue
    (loop [agent-characters
           (map agent character-plus-delta)
           processed []]
      ;; next we move their deferred into current queue
      (let [integrated-characters (map #(send %1 integrate-character
                                                    environ)
                                             agent-characters)]
        (map await integrated-characters)
        (let [fin-unfin (transform/split-fin-unfin integrated-characters)]
          (if (empty? (:not-done fin-unfin))
            (concat (:done fin-unfin) processed)
            (recur (:not-done fin-unfin) (:done fin-unfin))
            )
          )))
    )
  )
