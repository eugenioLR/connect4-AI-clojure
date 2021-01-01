(ns connect4AI.core (:require [connect4AI.AI :refer :all]
                             [connect4AI.UI :refer :all])
                    (:gen-class))

(defn -main [& args]
    (prn (connect4-game human-move #(connect4-AI-move %1 %2 %3 1) true)))

;(prn (connect4-game #(connect4-AI-move %1 %2 %3 3) #(connect4-AI-move %1 %2 %3 3) true))
;(prn (/ (count (for [i (range 100) :when (= (get (connect4-game random-move #(connect4-AI-move %1 %2 %3 4) false) :winner) 1)] i)) 100.0))
;(prn (/ (count (for [i (range 100) :when (= (get (connect4-game #(connect4-AI-move %1 %2 %3 2)
;                                                                #(connect4-AI-move %1 %2 %3 2) false) :winner) 1)] i)) 100.0))