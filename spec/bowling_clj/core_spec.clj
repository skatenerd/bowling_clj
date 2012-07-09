(ns bowling-clj.core-spec
  (:use [speclj.core]
        [bowling-clj.core]))

(describe "acceptance"
  (it "scores a zero game to zero"
    (let [rolls (repeat 20 0)
          score (score-game rolls)]
      (should= 0 score)))

  (it "scores a game of all ones to 20"
    (let [rolls (repeat 20 1)
          score (score-game rolls)]
      (should= 20 score))
  ) 

  (it "scores perfect game"
    (let [rolls [10 10 10 10 10 10 10 10 10 10 10 10]
          score (score-game rolls)]
      (should= 300 score)))

  (it "scores game ending in spare"
    (let [rolls [10 10 10 10 10 10 10 10 10 9 1 9]
          score (score-game rolls)]
      (should= (+ 
                (* 7 30)
                29
                20
                19)
               score)))

)

(describe "game state update"
  (describe "frame update"
    (it "adds a new frame"
      (let [game-state {:frames [[1 5]] :score 0}
            updated-state (update-state game-state 5)
            new-frames (:frames updated-state)]
        (should= [[1 5] [5]] new-frames))
    ) 
    (it "adds onto existing frame"
      (let [game-state {:frames [[1]] :score 0}
            updated-state (update-state game-state 5)
            new-frames (:frames updated-state)]
        (should= [[1 5]] new-frames))

    ) 

    (it "adds a new frame for strike"
      (let [game-state {:frames [[10]] :score 0}
            updated-state (update-state game-state 5)
            new-frames (:frames updated-state)]
        (should= [[10][5]] new-frames))) 
  )

  (describe "score update"
    (it "adds contribution from second throw in frame"
      (let [game-state {:frames [[1]]
                        :score 72}
            updated-state (update-state game-state 5)
            new-score (:score updated-state)]
        (should= 77 new-score))
    ) 
    (it "adds spare contribution"
      (let [game-state {:frames [[1 9]]
                        :score 72}
            updated-state (update-state game-state 5)
            new-score (:score updated-state)]
        (should= 82 new-score))
    ) 
    (it "does not add extraneous spare contribution"
      (let [game-state {:frames [[1 9] [4]]
                        :score 10}
            updated-state (update-state game-state 5)
            new-score (:score updated-state)]
        (should= 15 new-score))
    ) 
    (it "adds extra for a strike"
      (let [game-state {:frames [[10]]
                        :score 10}
            updated-state (update-state game-state 5)
            new-score (:score updated-state)]
        (should= 20 new-score)))

    (it "adds extra for two throws after strike"
      (let [game-state {:frames [[10][10]]
                        :score 20}
            updated-state (update-state game-state 5)
            new-score (:score updated-state)]
        (should= 35 new-score)))
  )) 

