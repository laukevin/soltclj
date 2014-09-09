(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))
(defn seq-contains? [coll target] (some #(= target %) coll))

(defstruct card :rank :suit)
(def suits [:spade :heart :club :diamond])
(def rank-order [:ace 7 8 9 10 :jack :queen])
(def rank-map {:ace 0 7 1 8 2 9 3 10 4 :jack 5 :queen 6})
(def rank-map-reverse {0 :ace 1 7 2 8 3 9 4 10 5 :jack 6 :queen})
(def ranks [7 8 9 10 :jack :queen :ace])
(def deck (shuffle (for [x ranks y suits] (struct card x y))))
(def pile {:foundation [] :reserve []})

;; helper methods to access the piles
(defn get-last-foundation [pile] ((last (pile :foundation)) :rank))
(defn get-last-reserve [pile] ((last (pile :reserve)) :rank))
(defn put-card-foundation [pile card] {:foundation (into (pile :foundation) [card]) :reserve (pile :reserve)})
(defn put-card-reserve [pile card] {:reserve (into (pile :reserve) [card]) :foundation (pile :foundation)})

(def game-piles (repeat 4 pile))

(defn add-reserve-pile [pile-card-pair]
  (cond (= [] (second pile-card-pair)) (first pile-card-pair)
        :else (put-card-reserve (first pile-card-pair) (second pile-card-pair))))

(defn draw-cards [piles deck]
  (map add-reserve-pile (map vector piles (take 4 (into (into [] deck) [[] [] [] []])))))

(defn reset-deck [piles]
  (reduce (fn [x y] [] (into x (take 4 y))) piles))

(defn clear-reserves [piles]
  (map (fn [x] {:reserve [] :foundation (x :foundation)}) piles))

(defn has-rank-pile? [rank pile]
  (cond (= [] (pile :reserve)) false
        :else (= rank (get-last-reserve pile))))

(defn has-rank-piles? [rank piles]
  (some (partial has-rank-pile? rank) (into [] piles)))

(defn has-queen-piles? [piles] (has-rank-piles? :queen piles))

(defn in-order-pile? [won-or-not pile] (and won-or-not (= pile rank-order)))

(defn has-won? [piles] (reduce in-order-pile? true piles))

(defn has-action-pile [pile]
  (cond (= [] (pile :reserve)) false
        (= (pile :foundation) []) (has-rank-pile? :ace pile)
        (= ((last (pile :foundation)) :rank) :queen) false
        :else (has-rank-pile? (get rank-map-reverse (inc (get rank-map ((last (pile :foundation)) :rank)))) pile)))

(defn has-action? [piles]
  (seq-contains? (into [] (map has-action-pile piles)) true))

(def app-state (atom {:game-piles game-piles
                      :deck deck}))

(defn place-card-on-foundation [pile]
  {:foundation [into (pile :foundation) [(last (pile :reserve))]] :reserve (cond (> 2 (count (pile :reserve))) [] :else (butlast (pile :reserve)))})

(defn create-new-piles [piles index new-pile]
  (map
    (fn [x]
      (cond (= index (.indexOf piles x)) new-pile
            :else x)) piles))

(defn user-action [app-state]
  (let [mut-game-piles (into [] (@app-state :game-piles)) mut-deck (@app-state :deck)]
    (println mut-game-piles)(def input (parse-int (read-line)))
    (println (place-card-on-foundation (get mut-game-piles input)))
    (println (has-action-pile (get mut-game-piles input)))
    (cond (has-action-pile (get mut-game-piles input))
      (swap! app-state assoc :game-piles (create-new-piles mut-game-piles input (place-card-on-foundation (get mut-game-piles input)))))))

(defn start []
  (while (not (has-won? (@app-state :game-piles)))
    (cond (has-action? (@app-state :game-piles)) (user-action app-state)
          (< 0 (count (@app-state :deck))) (swap! app-state assoc :game-piles (draw-cards (@app-state :game-piles) (@app-state :deck))
                                                  :deck (drop 4 (@app-state :deck)))
          :else (swap! app-state assoc :game-piles (clear-reserves (@app-state :game-piles)) :deck (reset-deck (@app-state :deck))))))
