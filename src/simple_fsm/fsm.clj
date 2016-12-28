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
;;    (println "begin integration of character: " (:name character))
    (loop [[event & remaining] (:event-queue
                                prepped-character)           
           prepped-character prepped-character]
      (if (= nil event)
        (assoc-in prepped-character [:event-queue] [])
        (let [processed-character
              (process-character prepped-character event environ)]
  ;;        (println (str "integrating character: "
    ;;                    (:name character) " for event: "
      ;;                  (:event-type event)))
          (recur remaining processed-character)
          )
        )
      )
    ))



(defn integrate-environ
  [environ characters time-delta]
  "integrates the environ with character agents for a delta value and 
   returns awaited character agents (there will be no pending actions on 
   agents on return"
  ;; first all characters get energy delta added to them
  (loop [char-plus-delta          
         (map
          #(send %1 (partial transform/add-time-energy time-delta)) characters)
         
         processed []]
    (println "added time energy delta")
    ;; next we move their deferred into current queue
    (let [integrated-characters (map #(send-off %1 integrate-character
                                            environ)
                                     char-plus-delta)]
      (doall (map #(await-for 1000 %1) integrated-characters))
      (let [fin-unfin (transform/split-fin-unfin2 integrated-characters)]
        ;;(println (str "finunfin: " fin-unfin))
        (if (empty? (:not-done fin-unfin))
          (concat (:done fin-unfin) processed)
          (recur (concat (:not-done fin-unfin) (:done fin-unfin)) [])
          )
        )))
  )
  
