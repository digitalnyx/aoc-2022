(ns d3
  (:require
   [clojure.set :refer [intersection]]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;(def input (slurp (io/resource "d3ex.txt")))
(def input (slurp (io/resource "d3.txt")))

(def lower-case
  (map char (range (int \a) (inc (int \z)))))

(def upper-case
  (map char (range (int \A) (inc (int \Z)))))

(def ->priority
  (into {}
        (map-indexed (fn [idx itm] [itm (inc idx)])
                     (flatten [lower-case upper-case]))))

(def sacks
  (map #(split-at (/ (count %) 2) %)
       (str/split-lines input)))

(def duplicates
  (for [[comp-1 comp-2] sacks]
    (first (intersection (set comp-1) (set comp-2)))))

;; Part one result
(apply + (map ->priority duplicates))

(def fireteams
  (partition 3 (str/split-lines input)))

(def badges
  (for [[elf-1 elf-2 elf-3] fireteams]
    (first (intersection (set elf-1) (set elf-2) (set elf-3)))))

;; Part two result
(apply + (map ->priority badges))
