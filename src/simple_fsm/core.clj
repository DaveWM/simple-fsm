(ns simple-fsm.core)

(def initial-scene {:characters [{:name "Jules"
                                  :emotion :calm}
                                 {:name "Brett"
                                  :emotion :scared}]})


(defn act-on [scene action]
  [scene []])

(defn play-scene [scene actions]
  (if (empty? actions)
    scene
    (let [current-action (first actions)
          _ (println current-action)
          [updated-scene next-actions] (act-on scene current-action)]
      (recur updated-scene (concat (rest actions) next-actions)))))

(play-scene initial-scene [[:say-what "Jules"]])
