(ns saolsen.firemarshal.model
  (:require [clojure.math.combinatorics :as comb]))

(defn card
  "creates a card"
  [rank suit]
  {:rank rank :suit suit})

(def ranks #{:2 :3 :4 :5 :6 :7 :8 :9 :10 :J :Q :K :A})
(def suits #{:hearts :spades :diamonds :clubs})
(def cards (map (partial apply card) (comb/cartesian-product ranks suits)))

(def types [:high :pair :2pair :three :straight :flush :fullhouse
            :four :straight-flush :royal-flush])

