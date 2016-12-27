(ns simple-fsm.fsm-test
  (:require [clojure.test :refer :all])
  (:require [simple-fsm.transform :as transform])
  (:require [simple-fsm.fsm :as fsm]))


;; testing the core system is interesting
;; first want to test i can create multimethod routing for two
;; character types

(def waiter-transitions [{:old-state :waiting
                          :event :guest-arrived
                          :action
                          (fn [character event environ]
                            (println "seating guests")
                            character
                            )
                          :new-state :seating-guest
                          :time-cost 2 }                       
                        
                         {:old-state :seating-guest
                          :event :guest-seated
                          :action
                          (fn [character event environ]
                            (println "seated guests")
                            character
                            )
                          :new-state :waiting
                          :time-cost 2 }
                        ])

(fsm/create-process-methods :waiter waiter-transitions)

(deftest process-character-test
  (testing "move a character through two states"
    (let [waiter1
          (assoc-in
           (transform/create-character :waiter "waiter1" :waiting)
           [:time-energy] 5)]
      (let [seating-guests-waiter
            (fsm/process-character waiter1 { :event-type :guest-arrived } nil)]
        (is (= :seating-guest (:state seating-guests-waiter)))
        (let [waiting-waiter
              (fsm/process-character seating-guests-waiter
                                     { :event-type :guest-seated } nil)]
          (is (= :waiting (:state waiting-waiter)))
          (is (= 1 (:time-energy waiting-waiter)))
          )
        )
      )))



(deftest integrate-character-test
  (testing "essentially same as above, but use integrate"
    (let [waiter1
          (assoc-in
           (transform/create-character :waiter "waiter1" :waiting)
           [:time-energy] 4)]
      (let [events [{:event-type :guest-arrived}
                    {:event-type :guest-seated }]]
        (let [processed-character
              (fsm/integrate-character
               (assoc-in waiter1 [:received-events] events)
               nil)]
          (is (= :waiting (:state processed-character)))
          (is (= 0 (:time-energy processed-character)))
          )
        )
      )))


(deftest process-character-no-energy-test
  (testing "move a character through two states"
    (let [waiter1
          (assoc-in
           (transform/create-character :waiter "waiter1" :waiting)
           [:time-energy] 3)]
      (let [seating-guests-waiter
            (fsm/process-character waiter1 { :event-type :guest-arrived } nil)]
        (is (= :seating-guest (:state seating-guests-waiter)))
        (let [waiting-waiter
              (fsm/process-character seating-guests-waiter
                                     { :event-type :guest-seated } nil)]
          (is (= :seating-guest (:state waiting-waiter)))
          (is (= 1 (:time-energy waiting-waiter)))          
          )
        )
      )))


;; testing integrate environ
;; add a guest transition
  
(def guest-transitions [
                        {:old-state :arrived
                         :event :have-a-seat
                         :action
                         (fn [character event environ]
                           (println "thank you")
                           character
                           )
                         :new-state :seated
                         :time-cost 3 }
                        ])

(fsm/create-process-methods :guest guest-transitions)

(deftest integrate-environ-test
  (testing "attempt to integrate multiple characters"
    (let [waiter1
          (assoc-in
           (transform/create-character :waiter "waiter1" :waiting)
           [:time-energy] 0)
          waiter2
          (assoc-in
           (transform/create-character :waiter "waiter2" :waiting)
           [:time-energy] 0)
          guest1
          (assoc-in
           (transform/create-character :guest "guest1" :arrived)
           [:time-energy] 0)
          ]
      (let [
            w1_events [{:event-type :guest-arrived}
                       {:event-type :guest-seated }]
            w2_events [{:event-type :guest-arrived}
                       {:event-type :guest-seated }]
            g1_events [{:event-type :have-a-seat}]]
        (let [w1 (assoc-in waiter1 [:received-events] w1_events)
              w2 (assoc-in waiter2 [:received-events] w2_events)
              g1 (assoc-in guest1 [:received-events] g1_events)]
          (let
              [characters (fsm/integrate-environ nil [w1 w2 g1] 2)]
            )
          )
        )
      )
    ))
