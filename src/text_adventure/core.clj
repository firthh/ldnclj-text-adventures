(ns text-adventure.core
  (:gen-class))

(def rooms
  {:hallway {:east :sauna
             :north :bedroom
             :description "Here be dragons.. in a painting on the wall"}
   :sauna {:west :hallway
           :description "This is the sauna, with some chains in the corner, and a fat man lying sleeping in the corner"
           :objects #{:dungeon-key}}
   :bedroom {:south :hallway
             :north :dungeon
             :description "The room is empty"}
   :dungeon {:south :hallway
             :locked true
             :description "A dark dimly lit brick room"}})

(defn can-enter? [room-key inventory]
  (cond
    (not (:locked (room-key rooms))) room-key
    (inventory (keyword (str (name room-key) "-key"))) room-key
    :else :locked))

(defn move-direction [room direction inventory]
  (if-let [new-room (direction (rooms room))]
    (can-enter? new-room inventory)))

(defn move-room [state direction]
  (if-let [new-room (move-direction (:room state) direction (:inventory state))]
    (if (= new-room :locked)
      ["The room was locked and you don't have the key" state]
      ["You moved rooms" (update state :room (constantly new-room))])
    ["There wasn't a room in that direction" state]))

(defn pickup-object [{:keys [room] :as state} object-name]
  (if-let [object (get (:objects (rooms room)) object-name)]
    [(str "Picked up " object) (update state :inventory conj object)]
    ["couldn't find anything" state]))

(defn command-person [command {:keys [room] :as state}]
  (case command
    "move east" (move-room state :east)
    "move west" (move-room state :west)
    "move north" (move-room state :north)
    "move south" (move-room state :south)
    "pickup dungeon key" (pickup-object state :dungeon-key)
    "suicide" ["you are dead" (assoc state :alive false)]
    "exit" ["goodbye" (assoc state :alive false)]
    ["I couldn't understanted me to do" state])

  ;return new state
  )

;; ------------------

(defn describe [state]
  (println (str "You're currently in the " (name (:room state)) " - " (->> state :room (get rooms) :description)))
  (if-let [objects (seq (:objects (rooms (:room state))))]
    (println "You can see: " objects)))

(defn -main []
  (loop [state {:room :hallway
                :alive true
                :inventory #{}}]
    (when (:alive state)
      (describe state)
      (println "What's your next command")
      (let [command (read-line)
            [message new-state] (command-person command state)]
        (println message)
        (recur new-state)))))
