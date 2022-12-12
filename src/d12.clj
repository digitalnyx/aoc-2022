(ns d12
  (:require
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

;(def input (slurp (io/resource "d12ex.txt")))
(def input (slurp (io/resource "d12.txt")))

(def grid
  (->> (str/split input #"\n")
       (mapv vec)))

(def inf Integer/MAX_VALUE)

(def height (count grid))
(def width (count (first grid)))

;; Also ended up being overkill
(def topo
  (zipmap
   (concat [\S] (mapv char (range 97 123)) [\E])
   (range)))

(defn neighbors
  [coords]
  (let [[x y] coords]
    [[x (+ y 1)] ; N
     [x (- y 1)] ; S
     [(+ x 1) y] ; E
     [(- x 1) y] ; W
     ]))
(def m-neighbors (memoize neighbors))

;; I thought I would need to know the distance delta
;; I did not, could simplify this
(defn e-dist
  [ce ne]
  (let [d (- (get topo ne inf)
             (get topo ce))]
    (cond
      (= d 1) d
      (< d 1) (+ 2 (abs d))
      :default inf)))
(def m-e-dist (memoize e-dist))

(defn adj
  [node]
  (for [n (m-neighbors node)
        :let [ce   (get-in grid node)
              ne   (get-in grid n)
              dist (m-e-dist ce ne)]
        :when (and (not (nil? ne))
                   (< dist inf))]
    n))
(def m-adj (memoize adj))

(def start-end
  (apply
   merge
   (for [x (range height)
         y (range width)
         :let [c (get-in grid [x y])]
         :when (or (= \S c) (= \E c))]
     {c [x y]})))

(def all-a-coords
  (for [x (range height)
        y (range width)
        :let [c (get-in grid [x y])]
        :when (= \a c)]
    [x y]))

;; Interested in a cleaner way to track depth
(defn bfs
  [start end]
  (loop [q (conj PersistentQueue/EMPTY [start 0]) v #{} depth 0]
    (if (or (empty? q) (= (first (peek q)) end))
      (when-not (empty? q) [depth start end])
      (let [[n step] (peek q)
            u (filter (complement v) (m-adj n))
            us (map (fn [x] [x (inc step)]) u)]
        ;(println (repeat step "*") "node:" n "neighbors:" u)
        (recur (apply conj (pop q) us) (apply conj v u) step)))))

;; Part one result
(bfs (get start-end \S) (get start-end \E))

;; Part two result
;(time
 (->>
  (for [s all-a-coords
        :let [e (get start-end \E)]]
    [s e])
  (pmap #(bfs (first %) (second %)))
  (filter (complement nil?))
  (sort-by first)
  (take 1))
 ;)
