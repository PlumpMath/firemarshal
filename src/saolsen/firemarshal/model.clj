(ns saolsen.firemarshal.model
  (:refer-clojure :exclude [==])
  (:require [clojure.math.combinatorics :as comb]
            [clojure.core.logic :refer :all]))

;; naive evaluator

;; Typical poker hand evaluators are very optomized and use large
;; lookup tables. I don't plan to run monte carlo simulations with my
;; evaluator and will just use a simple naive evaluator instead. If it
;; turns out to not be fast enough I can replace it later.

;; The idea is to create an equivalence relation between possible
;; hands and integers that represent the strength of the hand where
;; the higher the number the better the hand. For instance all the
;; hands that are a pair of 6's can be thought of as the same strength
;; nomatter what the other cards in the hand are. This can then be
;; used to tell which hand is the winner when comparing multiple
;; hands. Comparing 169 equivalence classes is much easier then
;; comparing 133,784,560 different possible hands.

;; The equivalence class number is between 0 and 168
;; Some of these classes are actually impossible to have hands in but
;; it's a lot simpler to set it up this way instead of trying to
;; figure out which ones are impossible and compressing it. For
;; instance we can't have a high-card hand where the high card is a 3
;; because there are 7 cards total and no strait or pairs of any kind.
;; It's easier to just assume there are 14 possible high-card hands
;; though.

(defn card [rank suit]
  {:rank rank :suit suit})

(def ranks [:2 :3 :4 :5 :6 :7 :8 :9 :10 :J :Q :K :A])
(def suits #{:hearts :spades :diamonds :clubs})
(def cards (map (partial apply card) (comb/cartesian-product ranks suits)))
(def types [:high :pair :2pair :three :straight :flush
            :fullhouse :four :strait-flush :royal-flush])

(defn rank-compare
  [a b]
  (compare (.indexOf ranks a)
           (.indexOf ranks b)))

(def classes
  (let [c (comb/cartesian-product ranks ranks)
        combs (->> c
                   (map vec)
                   (filter (fn [[f s]] (not= f s)))
                   (map #(sort rank-compare %))
                   (distinct))
        combine-for-id (fn [hand-type indicators]
                         [hand-type indicators])
        gen-seq (fn [[class s]]
                  (map (partial combine-for-id class) s))]
    (apply concat (map gen-seq [[:high ranks]
                                [:pair ranks]
                                [:2pair combs]
                                [:three ranks]
                                [:straight ranks]
                                [:flush ranks]
                                [:four ranks]
                                [:straight-flush (butlast ranks)]
                                [:royal-flush [:it]]]))))

(def class-map
  (->> classes       
       (map-indexed vector)
       (reduce (fn [m [i c]] (assoc m c i)) {})))



;; Helpers

;; (defn strait?
;;   "Determines if the hand is a strait."
;;   [cards]
;;   (let [ranks (map :rank cards)
;;         strait-finder (fn [chains current]
;;                         (if 


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
   
