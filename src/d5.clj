(ns d5
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

;(def input (slurp (io/resource "d5ex.txt")))
(def input (slurp (io/resource "d5.txt")))

(def crate-strings
  (->
   (str/split input #"\n\n")
   first
   (str/split #"\n")
   reverse))

(def move-strings
  (->
   (str/split input #"\n\n")
   second
   (str/split #"\n")))

(defn ->move
  [s]
  (map
   parse-long
   (-> s
       (str/replace "move " "")
       (str/replace "from " "")
       (str/replace "to "   "")
       (str/split #" "))))

(def num-stacks
  (->>
   (into '() (first crate-strings))
   (filter #(not= \space %))
   count
   inc))

(def stack-crate-pairs
  (for [stack  (range num-stacks)
        height (range (count (rest crate-strings)))
        :let [col   (.indexOf (first crate-strings) (str stack))
              row   (nth (rest crate-strings) height)
              crate (nth row col nil)]
        :when (and (not (nil? crate))
                   (not= \space crate))]
    [stack crate]))

(def initial-stacks
  (vec
   (for [stack (range num-stacks)
         :let [f #(when (= stack (first %)) (second %))]]
     (filterv some? (map f stack-crate-pairs)))))

(def moves-9k
  (apply
   concat
   (for [s move-strings
         :let [move (->move s)
               [cnt frm to] move]]
     (repeat cnt [frm to]))))

(defn execute-move-9k
  [stacks move]
  (let [[frm to] move]
    (if-let [crate (peek (nth stacks frm))]
      (-> stacks
          (update-in [frm] pop)
          (update-in [to] conj crate))
      stacks)))

;; Part one result
(apply str (map last (reduce execute-move-9k initial-stacks moves-9k)))

(def moves-9k1
  (for [s move-strings
        :let [move (->move s)
              [cnt frm to] move]]
    [cnt frm to]))

(defn execute-move-9k1
  [stacks move]
  (let [[cnt frm to] move
        break-at (- (count (nth stacks frm)) cnt)
        crates (drop break-at (nth stacks frm))
        rem-crates (fn [stack n] (vec (take n stack)))
        add-crates (fn [stack crates] (vec (concat stack crates)))]
    (-> stacks
        (update-in [frm] rem-crates break-at)
        (update-in [to] add-crates crates))))

;; Part two result
(apply str (map last (reduce execute-move-9k1 initial-stacks moves-9k1)))
