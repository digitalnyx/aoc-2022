(ns d9
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;(def input (slurp (io/resource "d9ex.txt")))
;(def input (slurp (io/resource "d9ex-2.txt")))
(def input (slurp (io/resource "d9.txt")))

(def knot-len 1)
(def stretch-limit (* knot-len (Math/sqrt 2)))

(def ->direction
  {"U" :up
   "D" :down
   "L" :left
   "R" :right})

(defn ->step
  [s]
  (let [[c d] (str/split s #" ")]
    (repeat (parse-long d) (->direction c))))

(def kw-steps
  (->> (str/split input #"\n")
       (map ->step)
       (apply concat)))

(defn move-head
  [head direction]
  (let [[x y] head]
    (cond
      (= direction :up)    [x (inc y)]
      (= direction :down)  [x (dec y)]
      (= direction :left)  [(dec x) y]
      (= direction :right) [(inc x) y])))

(defn kw-step-reducer
  [acc kw-step]
  (conj acc (move-head (last acc) kw-step)))

(def initial-head-steps
  (reduce kw-step-reducer [[0 0]] kw-steps))

(defn neighbors
  [coords]
  (let [[x y] coords]
    [[x (+ y knot-len)] ; N
     [x (- y knot-len)] ; S
     [(+ x knot-len) y] ; E
     [(- x knot-len) y] ; W
     [(- x knot-len) (+ y knot-len)] ; NW
     [(+ x knot-len) (+ y knot-len)] ; NE
     [(- x knot-len) (- y knot-len)] ; SW
     [(+ x knot-len) (- y knot-len)] ; SE
     ]))
(memoize neighbors)

(defn distance
  [node-1 node-2]
  (let [[x1 y1] node-1
        [x2 y2] node-2]
    (Math/sqrt
     (+
      (Math/pow (abs (- x2 x1)) 2)
      (Math/pow (abs (- y2 y1)) 2)))))
(memoize distance)

(defn closest-neighbor
  [coords neighbors]
  (->>
   (map-indexed
    (fn [_ itm]
      [itm (distance coords itm)]) neighbors)
   (sort-by second)
   (ffirst)))

(defn tail-steps
  [coll]
  (loop
   [steps (rest coll)
    tail (first coll)
    visited [tail]]
    (if (empty? steps)
      visited
      (let [head (first steps)]
        (if (> (distance head tail) stretch-limit)
          (let [tail (closest-neighbor head (neighbors tail))]
            (recur (rest steps) tail (conj visited tail)))
          (recur (rest steps) tail visited))))))

;; Part one result
(->> initial-head-steps
    (tail-steps)
    (set)
    (count))

(defn nth-tail-steps
  [n steps]
  (loop [steps steps nth 0]
    (if (= n nth)
      steps
      (recur (tail-steps steps) (inc nth)))))

;; Part two result
(->> initial-head-steps
    (nth-tail-steps 9)
     (set)
     (count))
