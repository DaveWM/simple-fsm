(ns simple-fsm.core)

(def initial-scene {:characters {"Jules" {:emotion :calm
                                          :alive true}
                                 "Brett" {:emotion :scared
                                          :alive true}}})

(defn kill-character [scene to-kill]
  (update-in scene [:characters to-kill :alive] (constantly false)))

(defn get-actions [scene [action [from-character to-character]]]
  (case action
        :say-what [[:shoot [to-character from-character]]]
        []))

(defn update-scene [scene [action [from-character to-character]]]
  (case action
    :shoot (kill-character scene to-character)
    scene))

(defn play-scene [scene [current-action & rest-actions]]
  (println current-action)
  (if (nil? current-action)
    scene
    (let [[updated-scene next-actions] ((juxt update-scene get-actions) scene current-action)]
      (recur updated-scene (concat rest-actions next-actions)))))

(play-scene initial-scene [[:say-what ["Brett" "Jules"]]])
