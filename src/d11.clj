(ns d11
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; I did not figure out the P2 trick on my own, good explanation here:
;; https://github.com/jake-gordon/aoc/blob/main/2022/D11/Explanation.md
;; Basically, you can get the prod of all the 'operator' nums for each
;; Monkey and then do 'item_worry % N' on each item. Doing this will
;; reduce the values but still allow the items to be thrown to the
;; correct monkey.

;(def input (slurp (io/resource "d11ex.txt")))
(def input (slurp (io/resource "d11.txt")))

(defn int-at
  [idx s]
  (parse-long (.substring s idx)))

(defn inta-at
  [idx s]
  (let [s (.substring s idx)]
    (mapv parse-long (str/split s #", "))))

(defn ->operation
  [s]
  (let [[l o r] (str/split s #" ")]
    (eval
     (read-string
      (str "(fn [old] (" o " " l " " r "))")))))

(defn ->monkey
  [lns]
  (sorted-map
   :items
   (inta-at 18 (nth lns 0))
   :operation
   (->operation
    (.substring ^String (nth lns 1) 19))
   :test
   (fn [_ item]
     (if (zero? (mod item (int-at 21 (nth lns 2))))
       [(int-at 29 (nth lns 3)) item]
       [(int-at 30 (nth lns 4)) item]))
   :inspections 0
   :denominator (int-at 21 (nth lns 2))
   ))

(defn distribute-items
  [monkeys items]
  (loop [items items monkeys monkeys]
    (if (empty? items)
      monkeys
      (let [[m i] (first items)]
        (recur (rest items)
               (update-in monkeys [m :items] conj i))))))

(defn do-round
  [coll]
  (loop [idx 0 monkeys (:monkeys coll)]
    (if (= idx (count monkeys))
      (assoc coll :monkeys monkeys)
      (let [m (get monkeys idx)
            items (->> (map (:operation m) (:items m))
                       (map (:worry-fn coll))
                       (map-indexed (:test m)))
            empty-monkey (-> (assoc m :items [])
                             (update-in [:inspections] + (count items)))
            updated-monkeys (distribute-items monkeys items)]
        (recur (inc idx) (merge updated-monkeys {idx empty-monkey}))))))

(defn calc-N
  [monkeys]
  (apply * (map (fn [[_ v]] (:denominator v)) monkeys)))

(def monkeys
  (loop [coll {} s (str/split input #"\n\n")]
    (if (empty? s)
      coll
      (let [lns (str/split-lines (first s))
            num (parse-long (str (nth (seq (first lns)) 7)))]
        (recur (assoc coll num (->monkey (rest lns))) (rest s))))))

;; Part one result
(->> {:monkeys monkeys
      :worry-fn
      #(long (Math/floor (/ % 3.0)))}
     (iterate do-round)
     (take (inc 20))
     (last)
     (:monkeys)
     (map #(:inspections (second %)))
     (sort)
     (take-last 2)
     (apply *))

;; Part two result
(->> {:monkeys monkeys
      :worry-fn
      #(mod % (calc-N monkeys))}
     (iterate do-round)
     (take (inc 10000))
     (last)
     (:monkeys)
     (map #(:inspections (second %)))
     (sort)
     (take-last 2)
     (apply *'))
