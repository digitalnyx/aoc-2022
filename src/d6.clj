(ns d6
  (:require
   [clojure.java.io :as io]
   [net.cgrand.xforms :as xf]))

;(def input (slurp (io/resource "d6ex.txt")))
(def input (slurp (io/resource "d6.txt")))

(def preamble-size 4)
(def message-size 14)

(def preamble-window
  (xf/partition preamble-size 1))

(def message-window
  (xf/partition message-size 1))

(def match
  (take-while #(not (apply distinct? %))))

;; Part one result
(->
 (sequence (comp preamble-window match) (seq input))
 count
 (+ preamble-size))

;; Prat 2 result
(->
 (sequence (comp message-window match) (seq input))
 count
 (+ message-size))
