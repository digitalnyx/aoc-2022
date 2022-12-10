(ns d9
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [criterium.core :refer [bench]]))

;(def input (slurp (io/resource "d9ex.txt")))
;(def input (slurp (io/resource "d9ex-2.txt")))
(def input (slurp (io/resource "d9.txt")))

(defn ->step
  [s]
  (let [[c d] (str/split s #" ")]
    (repeat (parse-long d) c)))

(def raw-steps
  (->> (str/split input #"\n")
       (map ->step)
       (apply concat)))

(defn move-head
  [head direction]
  (let [[x y] head]
    (cond
      (= direction "U") [x (inc y)]
      (= direction "D") [x (dec y)]
      (= direction "L") [(dec x) y]
      (= direction "R") [(inc x) y])))

(defn step-reducer
  [acc step]
  (conj acc (move-head (last acc) step)))

(def head-steps
  (reduce step-reducer [[0 0]] raw-steps))

(defn next-tail
  [head tail]
  (let [diff (map - head tail)]
    (when (> (apply max (map abs diff)) 1)
      (map + tail (map #(Integer/signum %) diff)))))
(def memo-next-tail (memoize next-tail))

(defn tail-steps
  [head-steps]
  (loop
   [steps (rest head-steps)
    tail (first head-steps)
    visited [tail]]
    (if (empty? steps)
      visited
      (if-let [tail (memo-next-tail (first steps) tail)]
        (recur (rest steps) tail (conj visited tail))
        (recur (rest steps) tail visited)))))

;; Part one result
(->> head-steps
    (tail-steps)
    (set)
    (count))

(defn nth-tail-steps
  [n head-steps]
  (loop [steps head-steps nth 0]
    (if (= n nth)
      steps
      (recur (tail-steps steps) (inc nth)))))

;; Part two result
;(bench
(->> head-steps
    (nth-tail-steps 9)
     (set)
     (count))
;)
