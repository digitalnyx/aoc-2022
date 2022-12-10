(ns d10
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;(def input (slurp (io/resource "d10ex.txt")))
(def input (slurp (io/resource "d10.txt")))

(def ops
  (->> (str/split-lines input)
       (map #(str/split % #" "))))

(defn noop
  [cycles x-reg]
  (conj cycles x-reg))

(defn addx
  [cycles n x-reg]
  [(vec (concat cycles [x-reg x-reg])) (+ x-reg n)])

(def cpu
  (loop
   [ops ops cycles [nil] x-reg 1]
    (if (empty? ops)
      (map-indexed vector cycles)
      (let [[op n] (first ops)]
        (if (= "addx" op)
          (let [[cycles x-reg] (addx cycles (parse-long n) x-reg)]
            (recur (rest ops) cycles x-reg))
          (recur (rest ops) (noop cycles x-reg) x-reg))))))

;; Part one result
(->> cpu
     (drop 20)
     (partition 1 40)
     (map flatten)
     (map #(apply * %))
     (apply +))

(defn draw-crt
  [rows]
  (doseq [row rows]
    (println (apply str row))))

;; Part two result
(->>
 (for [[cycle x-reg] (rest cpu)
       :let [pixel-pos (mod (dec cycle) 40)
             sprite-pos (range (dec x-reg) (inc (inc x-reg)))]]
   (if (>= (.indexOf sprite-pos pixel-pos) 0) "#" " "))
 (partition 40)
 (draw-crt)
)
