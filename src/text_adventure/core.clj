(ns text-adventure.core
  (:gen-class))

(defn dungeon-proc [state]
  (if (> (rand 1) 0.9)
    {:message "You were eaten by a grue"
     :new-state (assoc-in state [:player :alive] false)}
    {:new-state state}))

(def rooms
  {:hallway {:east :sauna
             :north :bedroom
             :description "Here be dragons.. in a painting on the wall"}
   :sauna {:west :hallway
           :description "This is the sauna, with some chains in the corner, and a fat man lying sleeping in the corner"}
   :bedroom {:south :hallway
             :north :dungeon
             :description "The room is empty"}
   :dungeon {:south :hallway
             :locked true
             :description "It is pitch black. You are likely to be eaten by a grue"
             :proc dungeon-proc}})

(def items {:sauna #{:dungeon-key}})

(defn can-enter? [room-key inventory]
  (cond
    (not (:locked (room-key rooms))) room-key
    (inventory (keyword (str (name room-key) "-key"))) room-key
    :else :locked))

(defn move-direction [room direction inventory]
  (if-let [new-room (direction (rooms room))]
    (can-enter? new-room inventory)))

(defn move-room [state direction]
  (if-let [new-room (move-direction (-> state :player :room) direction (-> state :player :inventory))]
    (if (= new-room :locked)
      ["The room was locked and you don't have the key" state]
      ["You moved rooms" (assoc-in state [:player :room] new-room)])
    ["There wasn't a room in that direction"
     state]))

(defn pickup-object [{:keys [player world] :as state} object-name]
  (if-let [object (get ((:items world) (:room player)) object-name)]
    [(str "Picked up " object)
     (-> state
         (update-in [:player :inventory] conj object)
         (update-in [:world :items (:room player)] disj object))]
    ["couldn't find anything"
     state]))

(defn command-person [command state]
  (case command
    "move east" (move-room state :east)
    "move west" (move-room state :west)
    "move north" (move-room state :north)
    "move south" (move-room state :south)
    "pickup dungeon key" (pickup-object state :dungeon-key)
    "suicide" ["you are dead" (assoc-in state [:player :alive] false)]
    "exit" ["goodbye" (assoc-in state [:player  :alive] false)]
    ["I couldn't understanted me to do" state])

  ;return new state
  )

;; ------------------

(defn describe [{:keys [player world]}]
  (println (str "You're currently in the " (name (:room player)) " - " (->> player :room (get rooms) :description)))
  (if-let [objects (seq ((:items world) (:room player)))]
    (println "You can see: " objects)))

(defn -main []
  (loop [state {:player  {:room :hallway
                          :alive true
                          :inventory #{}
                          }
                :world   {:items items}}]
    (when (-> state :player :alive)
      (describe state)
      (println "What's your next command")
      (let [command (read-line)
            [message new-state] (command-person command state)]
        (println message)
        (recur
         (if-let [proc (:proc (rooms (-> new-state :player :room)))]
           (let [{:keys [message new-state]} (proc new-state)]
             (when message
               (println message))
             new-state)
           new-state))
        ))))
