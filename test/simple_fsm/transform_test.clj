(ns simple-fsm.transform-test
  (:require [clojure.test :refer :all])
  (:require [simple-fsm.transform :as transform]))

(deftest create-character-test
  (testing "check all required keys are present")
  (let [character
        (transform/create-character :waiter "waiter1" :waiting)]
    (is (= :waiter (:character-type character)))
    (is (= "waiter1" (:name character)))
    (is (= :waiting (:state character)))
    )
  )

(deftest add-to-queue-test
  (testing "should increase queue size by one"
    (let [result (transform/add-to-queue {:event-queue [ 1 ] } 2 :event-queue)]
      (is (= 2 (count result)))
      )))

(deftest to-queue-test
  (testing "adds an event to character queue"
    (let [character (transform/create-character nil nil nil)]
      (let [character+event (transform/to-queue
                             :event-queue character 1)] 
        (is (= 1 (count (get-in character+event [:event-queue] ))))
        )
      )
    ))

(deftest add-time-energy-test
  (testing "add time energy"
    (let [character (transform/add-time-energy {:time-energy 0} 20)]
      (is (contains? character :time-energy))
      (is (= 20 (:time-energy character)))
      )))
      
(deftest take-character-energy-test
  (testing "check character energy can be minused out"
    (let [character (transform/add-time-energy {:time-energy 0} 2)]
      (let [result (transform/take-character-energy character 3)]
        (is (= -1 (:time-energy result)))
        )
      )
    ))

(deftest empty-queue-test
  (testing "empty event queue"
    (let [character (transform/empty-queue {:event-queue [1 2]} :event-queue)]
      (is (empty? (:event-queue character)))
      )))

(deftest prep-character-test
  (testing "move received and deferred to event-queue"
    (let [character
          {
           :received-events [1 2]
           :deferred-events [3]
           :event-queue []
           }]
      (let [requeued (transform/prep-character character)]
        (is (= 3 (count (:event-queue requeued))))
        )
      )
    ))


(defn create-fin-unfin-agents []
  [(agent {:deferred-events []})
   (agent {:deferred-events [1]})
   (agent {:deferred-events []})
   ]
  )
  

(deftest split-fin-unfin-test
  (testing "split agents between completed and incomplete"
    (let [agents (create-fin-unfin-agents)]
      (let [fin-unfin (transform/split-fin-unfin agents)]
        (is (= 2 (count (:done fin-unfin))))
        (is (= 1 (count (:not-done fin-unfin))))
        )
      )
    ))

(deftest send-event-test
  (testing "check that you can add to an agents queue"
    (let [agent1 (agent {:received-events []})
          agent2 (agent {:received-events []})]
      (let [event {:send-response-fn
                   (partial transform/send-event agent2 agent1) :val 2}]
        (let [agent2_recvd (transform/send-event agent1 agent2 event)]
          (await agent2_recvd)
          (is (= 2 (:val (first (:received-events @agent2_recvd)))))
          )
        )
      )
    ))

(deftest fin-unfin-chute-test
  (testing "should split character agent and his value"
    (let [agent1 (agent {:deferred-events []})
          agent2 (agent {:deferred-events [1]})
          fin-unfin {:done (atom []) :not-done (atom [])}]
      (let [agent1_val (transform/fin-unfin-chute @agent1 agent1 fin-unfin)]
        (is (= 1 (count @(:done fin-unfin))))
        )
      (let [agent2_val (transform/fin-unfin-chute @agent2 agent2 fin-unfin)
            agent1_val (transform/fin-unfin-chute @agent1 agent1 fin-unfin)]
        (is (= 1 (count @(:not-done fin-unfin))))
        (is (= 2 (count @(:done fin-unfin))))
        )
      )
    ))
  
(deftest split-fin-unfin2-test
  (testing "split agents between completed and incomplete"
    (let [agents (create-fin-unfin-agents)]
      (let [fin-unfin (transform/split-fin-unfin2 agents)]
        (is (= 2 (count (:done fin-unfin))))
        (is (= 1 (count (:not-done fin-unfin))))
        )
      )
    ))

