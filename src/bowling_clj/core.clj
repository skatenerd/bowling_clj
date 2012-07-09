(ns bowling-clj.core)

(declare update-state get-new-frames add-new-frame? get-new-score get-contribution)

(def max-frames 10)

(defn safe-conj [a b]
  (conj (vec a) b))

(defn last-frame-spare [game-state]
  (and
    (= 10 (apply + (last (:frames game-state))))
    (= 2 (count (last (:frames game-state))))))

(defn last-roll-strike [game-state]
  (= 10 (last (flatten (:frames game-state)))))

(defn second-to-last-roll-strike [game-state]
  (and
    (>= (count (flatten (:frames game-state))) 2)
    (= 10 (last (pop (vec (flatten (:frames game-state))))))))

(defn score-game [rolls] 
  (let [initial-state {:score 0 :rolls []}]
    (:score (reduce update-state initial-state rolls))))

(defn update-state [game-state roll]
  (let [old-frames (:frames game-state)
        new-frames (get-new-frames old-frames roll)
        new-score (get-new-score game-state roll)]
    (assoc
      game-state
      :score
      new-score
      :frames
      new-frames
      )))

(defn get-new-score [game-state roll]
  (let [contribution (get-contribution game-state roll)]
  (+ (:score game-state) contribution)))

(defn last-roll-before-tenth-frame [frames]
  (<= (count frames) 10))

(defn roll-contribution? [game-state]
  (let [new-frames (get-new-frames (:frames game-state) 0)]
    (last-roll-before-tenth-frame new-frames)))

(defn spare-contribution? [game-state]
  (last-frame-spare game-state))

(defn strike-contribution? [game-state]
  (and
    (< (count (:frames game-state)) 11)
    (last-roll-strike game-state)))

(defn distant-strike-contribution? [game-state]
  (second-to-last-roll-strike game-state))

(defn get-contribution [game-state roll]
  (let [contributions
        [(roll-contribution? game-state)
         (spare-contribution? game-state)
         (strike-contribution? game-state)
         (distant-strike-contribution? game-state)]]
    (* roll
       (apply +
        (map
          #(if % 1 0)
          contributions)))))

(defn get-new-frames [old-frames roll]
  (if (add-new-frame? old-frames)
    (safe-conj old-frames [roll])
    (safe-conj (pop old-frames) (safe-conj (last old-frames) roll))))

(defn add-new-frame? [old-frames]
  (or
    (= 2 (count (last old-frames)))
    (= 10 (apply + (last old-frames)))))
