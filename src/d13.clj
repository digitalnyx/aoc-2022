(ns d13
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;(def input (slurp (io/resource "d13ex.txt")))
(def input (slurp (io/resource "d13.txt")))

(def pairs
  (->> (str/split input #"\n\n")
       (map str/split-lines)
       (map #(map read-string %))))

(defn comp-pair
  [[left right]]
  (let [fl (first left)
        fr (first right)]
    ;(println "Comparing:" left right)
    (cond
      ;; Both empty, equality
      (and (empty? left) (empty? right))
      nil
      ;; Left ran out of items
      (empty? left) true
      ;; Right ran out of items
      (empty? right) false
      ;; Both ints
      (and (int? fl) (int? fr))
      (cond
        ;; Left int is lower
        (< fl fr) true
        ;; Right int is lower
        (> fl fr) false
        ;; Left and Right are equal
        :default (comp-pair [(rest left) (rest right)]))
      ;; Left is an int and Right is a collection
      (and (int? fl) (coll? fr))
      (let [r (comp-pair [(cons [fl] (rest left)) right])]
        (if (nil? r)
          (comp-pair [(rest (rest left)) (rest right)])
          r))
      ;; Left is a collection and Right is an int
      (and (coll? fl) (int? fr))
      (let [r (comp-pair [left (cons [fr] (rest right))])]
        (if (nil? r)
          (comp-pair [(rest left) (rest (rest right))])
          r))
      ;; Left and Right are collections
      (and (coll? fl) (coll? fr))
      (let [r (comp-pair [fl fr])]
        (if (nil? r)
          (comp-pair [(rest left) (rest right)])
          r))
      :default (println "Unknown compare")
      )))

;; Part one result
(->>
 (for [p pairs]
   (comp-pair p))
 (map-indexed
  (fn [idx itm] [(inc idx) itm]))
 (filter #(true? (second %)))
 (map first)
 (apply +))

(defn packet-compare
  [x y]
  (case
   (comp-pair [x y])
   true -1
   false 1
   nil 0))

(def divs (list [[2]] [[6]]))

;; Part two result
(->>
 (for [x (range (count pairs))
       y (range 2)]
   (nth (nth pairs x) y))
 (into divs)
 (sort-by identity packet-compare)
 (map-indexed
  (fn [idx itm] [(inc idx) itm]))
 (filter #(some (set %) divs))
 (map first)
 (apply *))
