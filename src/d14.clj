(ns d14
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [criterium.core :refer [bench]]))

;(def input (slurp (io/resource "d14ex.txt")))
(def input (slurp (io/resource "d14.txt")))

(def paths
  (map #(read-string
         (str/replace
          (str "[[" % "]]") #" -> " "]["))
       (str/split-lines input)))

(defn path->rocks [[x1 y1] [x2 y2]]
  (for [n (range (min x1 x2) (inc (max x1 x2)))
        m (range (min y1 y2) (inc (max y1 y2)))]
    [n m]))

(defn grain-path
  [[x y] obstacles]
  (let [down [x (inc y)]
        dleft [(dec x) (inc y)]
        dright [(inc x) (inc y)]]
    (cond
      (not (contains? obstacles down)) down
      (not (contains? obstacles dleft)) dleft
      (not (contains? obstacles dright)) dright)))

(defn drop-sand
  [start rocks & {:keys [abyss floor]}]
    (loop [grain start obstacles rocks]
      (if (and abyss (> (second grain) abyss))
        obstacles
        (let [move (grain-path grain obstacles)
              floor? (= (second grain) floor)
              start? (= grain start)]
        (cond
          floor?   (recur start (conj obstacles grain))
          move     (recur move obstacles)
          start?   (conj obstacles grain)
          :default (recur start (conj obstacles grain)))))))

(def rocks
  (->> (map #(map path->rocks % (rest %)) paths)
       (apply concat)
       (apply concat)
       (set)))

(def abyss
  (->> (sort-by second rocks)
       (last)
       (second)))

;; Part one result
;; Execution time mean : 41.782408 ms
#_(bench
   (-
    (count (drop-sand [500 0] rocks :abyss abyss))
    (count rocks)))

;; Part two result
;; Execution time mean : 1.474061 sec
#_(bench
   (-
    (count (drop-sand [500 0] rocks :floor (inc abyss)))
    (count rocks)))

;; Perf improvement #1
;; I originally separated the sand and rocks into two sets.
;; drop-sand returned the sand list. However, this was causing
;; the grain path to merge every time.

;; Perf improvement #2
;; I was using two functions in the main loop, one to get sand
;; paths and the other was a predicate to filter out those paths
;; that had been filled. Running filter in the loop basically
;; made part two run past 20 minutes. Lesson learned.