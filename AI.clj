(ns connect4AI.AI (:require [connect4AI.game :as game]))
(require '[clojure.test :refer :all])

(defn gen-legal-moves [board]
    (for [x (range (count (get board 0)))
          :when (not (get (get (get board 0) x) :filled))]
        x))



;======= TESTS =========
(deftest legal-moves
    (def A {:player game/PLAYER-1 :filled true})
    (def B {:player game/PLAYER-2 :filled true})
    (def N {:player 0 :filled false})
    (def test-board-0 (game/empty-board 6))
    (def test-board-1 [[A N N A N B]
                       [A N B A N B]
                       [B A B A N B]
                       [B A A A N A]
                       [A B A B N B]
                       [A B A B N A]])
    (is (= (gen-legal-moves test-board-0) (range 6))
    (is (= (gen-legal-moves test-board-1) '(1 2 4)))))

(run-tests)
