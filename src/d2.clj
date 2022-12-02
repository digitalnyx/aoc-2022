(ns d2
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

;(def input (slurp (io/resource "d2ex.txt")))
(def input (slurp (io/resource "d2.txt")))

(def guide
  (map #(str/split % #" ")
    (str/split-lines input)))

(def ->shape
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(def rounds
  (map #(map ->shape %) guide))

(def points
  {:rock 1 :paper 2 :scissors 3 :win 6 :loss 0 :tie 3})

(def win-conditions
  [[:rock :scissors] [:paper :rock] [:scissors :paper]])

(defn calc-points
  [round]
  (let [[them you] round]
    (->
      (cond
        (= you them) (points :tie)
        (some #(= [you them] %) win-conditions) (points :win)
        :default (points :loss))
      (+ (points you)))))

;; Part one result
(def p1 (apply + (map calc-points rounds)))

(def ->win
  {:rock :paper
   :paper :scissors
   :scissors :rock})

(def ->lose
  {:rock :scissors
   :paper :rock
   :scissors :paper})

(def ->draw
  {:rock :rock
   :paper :paper
   :scissors :scissors})

(def ->strategy
  (merge ->shape
    {"X" ->lose
     "Y" ->draw
     "Z" ->win}))

(def roundz
  (for [[them outcome] guide
        :let [shape (->shape them)
              choice ((->strategy outcome) shape)]]
    [shape choice]))

;; Part two result
(def p2 (apply + (map calc-points roundz)))
