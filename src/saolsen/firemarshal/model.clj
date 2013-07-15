(ns saolsen.firemarshal.model
  (:refer-clojure :exclude [==])
  (:require [clojure.math.combinatorics :as comb]
            [clojure.core.logic :refer :all]))

(defn card
  "creates a card"
  [rank suit]
  {:rank rank :suit suit})

(def ranks #{:2 :3 :4 :5 :6 :7 :8 :9 :10 :J :Q :K :A})
(def suits #{:hearts :spades :diamonds :clubs})
(def cards (map (partial apply card) (comb/cartesian-product ranks suits)))

(def types [:high :pair :2pair :three :straight :flush :fullhouse
            :four :straight-flush :royal-flush])

;; Want to do a lot of this part with core.logic. Need to start by
;; trying to write relations for the different types of hands (I
;; think?)
(defn collect-ranks
  [cards]
  (let [rs (map :rank cards)
        base (reduce #(assoc %1 %2 0) {} ranks)]
    (reduce #(assoc %1 %2 (inc (%2 %1))) base rs)))

(defn count-check-pred
  "Used for predicates that look at the number of cards of a similar rank.
   c is the number of cards and num is the number of instances of
   that count."
  [c num cards]
  (let [ranks (collect-ranks cards)]
    (= num
       (count (filter #(= % c) (vals ranks))))))

(defn pair?
  [cards]
  (count-check-pred 2 1 cards))

(defn two-pair?
  [cards]
  (count-check-pred 2 2 cards))


;; FIXME: This isn't right, because we are looking at 7 cards.
(defn three?
  [cards]
  (count-check-pred 3 1 cards))

(defn four?
  [cards]
  (count-check-pred 4 1 cards))

(defn get-type [cards]
  (cond
   (pair? cards) :pair
   (two-pair? cards) :2pair
   :else :high))
   
