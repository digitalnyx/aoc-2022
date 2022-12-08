(ns d7
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as w]))

;(def input (slurp (io/resource "d7ex.txt")))
(def input (slurp (io/resource "d7.txt")))

(defn useful?
  [s]
  (when-not
   (or (str/starts-with? s "$ ls")
       (str/starts-with? s "dir "))
    true))

(defn parse-line
  [s]
  (cond
    (str/starts-with? s "$ cd ..")
    [:out nil]
    (str/starts-with? s "$ cd ")
    [:into (symbol (nth (str/split s #" ") 2))]
    :default
    (let [[size name] (str/split s #" ")]
      [name (parse-long size)])))

(def lines
  (->>
   (rest (str/split input #"\n"))
   (filter useful?)))

(def filesystem
  (loop
   [kys []
    m {}
    coll lines]
    (if (not-empty coll)
      (let [[k v] (parse-line (first coll))]
        ;(println kys m k v)
        (cond
          (= :out k) (recur (pop kys) m (rest coll))
          (= :into k) (recur (conj kys v) m (rest coll))
          :default (recur kys (assoc-in m (conj kys k) v) (rest coll))))
      m)))

(defn traverse
  ([m] (traverse m []))
  ([m path]
   (let [kys (filter symbol? (keys m))]
     (if (empty? kys)
       path
       (for [k kys]
         (traverse (k m) (conj path k)))))))

(defn flatten-traversals
 [acc coll]
  (if (vector? coll)
    (conj acc coll)
    (reduce flatten-traversals acc coll)
    ))

(defn traversal->endpoints
  [coll]
  (for [i (range 1 (inc (count coll)))]
    (vec (take i coll))))

(def paths
  (->>
   (traverse filesystem)
   (reduce flatten-traversals [])
   (map traversal->endpoints)
   (reduce concat [])
   (set)))

(defn fs+
  [acc coll]
  (let [[_ v] coll]
    (cond
      (int? v) (+ acc v)
      (map? v) (+ acc (reduce fs+ 0 v))
      :default acc)))

;; Part one result
(->>
 (for [path paths]
   [path (reduce fs+ 0 (get-in filesystem path))])
 (filter #(<= (second %) 100000))
 (w/walk second #(apply + %)))

(def disk-capacity 70000000)
(def df (- disk-capacity (reduce fs+ 0 filesystem)))
(def update-size 30000000)
(def cutoff (- update-size df))

;; Part two result
(->>
 (for [path paths]
   [path (reduce fs+ 0 (get-in filesystem path))])
 (filter #(>= (second %) cutoff))
 (map second)
 (sort)
 (first))
