(ns simple-fsm.core
  (:require [simple-fsm.fsm :as fsm])
  (:require [simple-fsm.transform :as transform])
  (:require [clojure.tools.logging :as log])
  )
                                       
;; pulp fiction example from meetup

(def jules (agent (transform/create-character :anti-hero "Jules" :calm)))
(def brett (agent (transform/create-character :victim "Brett" :agitated)))


(def jules-transitions [
                        {:old-state :calm
                         :event :what
                         :action
                         (fn [character event environ]
                           (println "say what mother fucker!?")
                           (transform/respond event :say-what-mf)
                           character
                           )
                         :new-state :angry
                         :time-cost 1}

                        {:old-state :angry
                         :event :what
                         :action
                         (fn [character event environ]
                           (println "die mother fucker")
                           
                           (transform/respond event :shoot-brett)
                           character
                           )
                         :new-state :angry
                         :time-cost 3 }

                        {:old-state :angry
                         :event :brett-dies
                         :action
                         (fn [character event environ]
                           (println "*Jules Smiles*")
                           character
                           )
                         :new-state :smiling
                         :time-cost 0 }
                        ])

(def brett-transitions [
                        {:old-state :agitated
                         :event :say-what
                         :action
                         (fn [character event environ]
                           (println "what..?")
                           (transform/respond event :what)
                           character
                           )
                         :new-state :scared
                         :time-cost 1
                         }
                        
                        {:old-state :scared
                         :event :say-what-mf
                         :action
                         (fn [character event environ]
                           (println "ermm, what..?")
                           (transform/respond event :what)
                           character
                           )
                         :new-state :shit-scared
                         :time-cost 4
                         }

                        {:old-state :shit-scared
                         :event :shoot-brett
                         :action
                         (fn [character event environ]
                           (println "ahhhh..!")
                           (transform/respond event :brett-dies)
                           character
                           )
                         :new-state :dead
                         :time-cost 0
                         }
                        ])

                                    

(fsm/create-process-methods :anti-hero jules-transitions)
(fsm/create-process-methods :victim brett-transitions)

;; boot strap event
(def boot-strap-event {:event-type :say-what
                      :send-response-fn
                      (partial transform/send-event brett jules)})

(transform/send-event jules brett boot-strap-event)

(defn -main
  "pulp fiction"
  [& args]
  (log/info "Simple FSM starting to run: ")
  (fsm/integrate-environ nil [jules brett] 9)
  (shutdown-agents )
  )


                        
