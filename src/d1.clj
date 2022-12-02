(ns d1
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

;(def input (slurp (io/resource "d1ex.txt")))
(def input (slurp (io/resource "d1.txt")))

(def elves
  (map
    #(map parse-long (str/split-lines %))
    (str/split input #"\n\n")))

(def elf-kcals
  (map #(reduce + %) elves))

;; Part one result
(def p1 (reduce #(if (> %1 %2) %1 %2) elf-kcals))

;; Part two result
(def p2 (apply + (take 3 (reverse (sort elf-kcals)))))
