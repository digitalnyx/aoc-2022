(ns d4
  (:require
   [clojure.java.io :as io]
   [clojure.set :refer [intersection]]
   [clojure.string :as str]))

;(def input (slurp (io/resource "d4ex.txt")))
(def input (slurp (io/resource "d4.txt")))

(defn ->ranged-elf-pair
  [elf-pair]
  (let [[elf-1 elf-2] elf-pair
        [e1s e1e] (map parse-long (str/split elf-1 #"-"))
        [e2s e2e] (map parse-long (str/split elf-2 #"-"))]
    (vector
     (vec (range e1s (inc e1e)))
     (vec (range e2s (inc e2e))))))

(def ranged-elf-pairs
  (map #(->ranged-elf-pair (str/split % #","))
       (str/split-lines input)))

(defn sections-overlap?
  [ranged-elf-pair]
  (let [[elf-1 elf-2] ranged-elf-pair]
    (or (every? true? (map #(.contains elf-1 %) elf-2))
        (every? true? (map #(.contains elf-2 %) elf-1)))))

;; Part one result
(count (filter true? (map sections-overlap? ranged-elf-pairs)))

;; Part two result
(count
 (for [[elf-1 elf-2] ranged-elf-pairs
   :when (not-empty (intersection (set elf-1) (set elf-2)))]
   true))
