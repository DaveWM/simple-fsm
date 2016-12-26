(ns simple-fsm.core
  (:require [simple-fsm.fsm :as fsm])
  (:require [simple-fsm.transform :as transform]))
                                       
;; now if an event reaches the fsm you want it to transition
;; so you would want to say create-fsm(character, valid-transitions,
;; pre-action, action, post-action.
;; so the engine would process events for character while it has time
;; energy

(def event1 { :num-guest 1 :event-type :guest-arrived })

(def waiter1 (transform/create-character :waiter "waiter1" :waiting))
(def guest1 (transform/create-character :guest "guest1" :arrived))

(def partial-send (partial transform/send-event guest1 waiter1)) 


(def waiter-transition [{:old-state :waiting :event :guest-arrived
                        :action
                        (fn [character event environ]
                          (let [num-guests (:num-guest event)]
                            (println (str "seating " num-guests " guests!"))
                            )
                          character 
                          ) :new-state :seating-guests
                         :time-cost 2.0 }                       
                        ])

(fsm/create-process-methods :waiter waiter-transition)
(fsm/create-process-methods :guest [])

;;(integrate-character waiter1 [event1 event1] nil)

