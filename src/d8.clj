(ns d8
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;(def input (slurp (io/resource "d8ex.txt")))
(def input (slurp (io/resource "d8.txt")))

(defn ->row
  [s]
  (mapv #(parse-long (str %)) (vec s)))

(def grid
  (->> (str/split input #"\n")
       (mapv ->row)))

(def height (count grid))
(def width (count (first grid)))

(defn north-of
  [coords]
  (let [[x y] coords]
    (for [i (reverse (range x))]
      (get-in grid [i y]))))

(defn south-of
  [coords]
  (let [[x y] coords]
    (for [i (range (inc x) height)]
      (get-in grid [i y]))))

(defn east-of
  [coords]
  (let [[x y] coords]
    (for [i (range (inc y) width)]
      (get-in grid [x i]))))

(defn west-of
  [coords]
  (let [[x y] coords]
    (for [i (reverse (range y))]
      (get-in grid [x i]))))

(defn neighbors
  [coords]
  (list
   (north-of coords)
   (south-of coords)
   (east-of coords)
   (west-of coords)))

(defn visible?
  [height neighbors]
  (let [f (fn [coll] (filter #(>= % height) coll))]
    (cond
      (some empty? neighbors) true
      (some empty? (map f neighbors)) true
      :default false)))

;; Part one result
(->>
 (for [x (range height)
       y (range width)
       :let [h (get-in grid [x y])
             n (neighbors [x y])]]
   (visible? h n))
 (filter true?)
 (count))

(defn take-until
  [pred coll]
  (when-let [s (seq coll)]
    (if (pred (first s))
      (cons (first s) nil)
      (cons (first s) (take-until pred (rest coll))))))

(defn scenic-score
  [height neighbors]
  (let [f (fn [coll] (take-until #(>= % height) coll))]
    (->> neighbors
         (map f)
         (map count)
         (apply *)
    )))

;; Part two result
(->>
 (for [x (range height)
       y (range width)
       :let [h (get-in grid [x y])
             n (neighbors [x y])]]
   (scenic-score h n))
 (sort)
 (last))
