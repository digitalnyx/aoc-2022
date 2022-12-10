(ns d9
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [criterium.core :refer [bench]]))

;(def input (slurp (io/resource "d9ex.txt")))
;(def input (slurp (io/resource "d9ex-2.txt")))
(def input (slurp (io/resource "d9.txt")))

(defn s->step
  [s]
  (let [[c d] (str/split s #" ")]
    (repeat (parse-long d) c)))

(defn step->coords
  [[x y] direction]
  (cond
    (= direction "U") [x (inc y)]
    (= direction "D") [x (dec y)]
    (= direction "L") [(dec x) y]
    (= direction "R") [(inc x) y]))

(def input-steps
  (let [rf #(conj %1 (step->coords (last %1) %2))]
    (->> (str/split input #"\n")
         (map s->step)
         (apply concat)
         (reduce rf [[0 0]]))))

(defn next-tail
  [head tail]
  (let [diff (map - head tail)]
    (when (> (apply max (map abs diff)) 1)
      (map + tail (map #(Integer/signum %) diff)))))
(def memo-next-tail (memoize next-tail))

(defn tail-steps

  ([head-steps]
   (loop
    [steps   (rest head-steps)
     tail    (first head-steps)
     visited [tail]]
     (if (empty? steps)
       visited
       (if-let [tail (memo-next-tail (first steps) tail)]
         (recur (rest steps) tail (conj visited tail))
         (recur (rest steps) tail visited)))))

  ([n head-steps]
   (loop [steps head-steps n n]
     (if (= 0 n)
       steps
       (recur (tail-steps steps) (dec n))))))

;; Part one result
(->> input-steps
     (tail-steps)
     (set)
     (count))

;; Part two result
;(bench
(->> input-steps
     (tail-steps 9)
     (set)
     (count))
;)