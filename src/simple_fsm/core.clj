(ns simple-fsm.core)
                                       
(def initial-scene {:characters {"Jules" {:emotion :calm
                                          :alive true}
                                 "Brett" {:emotion :scared
                                          :alive true}}})

(defmacro defactions [& body]
  (let [[def-name & funcs] body
        pairs (partition 2 funcs)]
    `(def ~def-name
       ~(mapv (fn [[pred get-actions]]
                [(if (keyword? pred)
                    (fn [scene action characters]
                      (= action pred))
                    pred)
                 (if (keyword? get-actions)
                   (fn [scene action characters character-names]
                     [[get-actions (reverse character-names)]])
                   get-actions)])
              pairs))))

(defactions action-definitions
  :say-what
  (fn [scene action [from-character to-character] character-names]
    (if (= :angry (:emotion to-character))
      [[:shoot (reverse character-names)]]
      [[:say-what-again-motherfucker (reverse character-names)]]))

  (fn [scene action [from-character to-character]]
      (and (= (:emotion to-character) :scared)
           (= (:emotion from-character) :angry)))
  :say-what)

(defn update-character [scene name key value]
  (assoc-in scene [:characters name key] value))

(defn some-character [scene pred]
  (some (fn [[name character]] (pred character)) (:characters scene)))

(defn characters-where [scene pred]
  (filter (fn [[name character]] (pred character)) (:characters scene)))

(defn valid-action? [scene [action [from to]]]
  (get-in scene [:characters from :alive]))

(defn kill-character [scene name]
  (-> scene
      (update-character name :alive false)
      (update-character name :emotion :dead)))

(defn get-actions [scene [action character-names]]
  (let [characters (map #(get-in scene [:characters %]) character-names)]
    (->> action-definitions
         (filter (fn [[pred _]]
                   (pred scene action characters)))
         (mapcat (fn [[_ get-actions]]
                   (get-actions scene action characters character-names))))))

(defn update-scene [scene [action [from-character to-character]]]
  (case action
    :say-what (update-character scene to-character :emotion :angry)
    :shoot (kill-character scene to-character)
    scene))

(defn play-scene [scene [current-action & rest-actions]]
  (println current-action)
  (if (nil? current-action)
    scene
    (let [[updated-scene next-actions] ((juxt update-scene get-actions) scene current-action)]
      (recur updated-scene (->> (concat rest-actions next-actions)
                                (filter (partial valid-action? updated-scene)))))))

(play-scene initial-scene [[:say-what ["Brett" "Jules"]]])
