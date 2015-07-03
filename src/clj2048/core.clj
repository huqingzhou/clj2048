(ns clj2048.core
  (:require [clj2048.view :as view]
            [clojure.tools.cli :refer [cli]])
  (:import [jline.console ConsoleReader])
  (:gen-class))
(def board-style "-")

;(def ^:dynamic *total-score* 0)
(def total-score (ref 0))
(def fake-move (ref false))

(defn set-fake-move [bool]
  (dosync (ref-set fake-move bool)))
(defn is-fake-move? []
  @fake-move)

(defn get-total-score []
  @total-score)

(defn update-score [score]
  (dosync (ref-set total-score (+ @total-score score))))

(defn new-board [w h]
  (into [] (repeat w (into []
                           (repeat h board-style)))))

;;add new random number
(defn random-row-with-empty [board]
  (let [row-count (count board)]
    (loop [failed-row-idx []]
      (if-not (= (count failed-row-idx) row-count)
        (let [rand-row-idx (rand-int row-count)]
          (if (>= (.indexOf failed-row-idx rand-row-idx) 0)
            (recur failed-row-idx)
            (if (>= (.indexOf (nth board rand-row-idx) board-style) 0)
              rand-row-idx
              (recur (conj failed-row-idx rand-row-idx)))))))))

(defn rand-num []
  ;(let [nums {0 2 1 4 2 8 3 16 4 32 5 64 6 128 7 256 8 512 9 1024 10 2048 11 4096}]
  (let [nums {0 2 1 4}]
    (nums (rand-int (count (seq nums))))))

(defn new-num-to-row [row]
  (let [empty-idx (remove nil?
                          (map-indexed
                           (fn [idx item] (if (= item board-style)
                                           [idx item]))
                           row))
        rand-idx (rand-int (count empty-idx))]
   (assoc row (first (nth empty-idx rand-idx)) (rand-num))))

(defn new-num [board]
  (let [row-idx (random-row-with-empty board)]
    (if row-idx
      (let [new-row (new-num-to-row (nth board row-idx))]
        (assoc board row-idx new-row))
      board)))

;;move
(defn vector-reverse [v]
  (into [] (reverse v)))

(defn transpose [m]
  (apply mapv vector m))
                                        ;
;;
;; split to more functions
;;
(defn combine-row [row]
  (let [c (count row)
        row-without-empty  (remove (fn [e] (if (= e board-style) true false)) row)
        merged-row (loop [reverse-row-without-empty (reverse row-without-empty)
                                    new-row []]
                               (let [to-be-merged (take 2 reverse-row-without-empty)]
                                 (if (seq to-be-merged)
                                   (if (= (count to-be-merged) 1)
                                     (recur (rest reverse-row-without-empty)
                                            (conj new-row (first to-be-merged)))
                                     (if (= (first to-be-merged) (second to-be-merged))
                                       (recur (rest  (rest reverse-row-without-empty))
                                              (conj new-row (do (if-not (is-fake-move?)
                                                                  (update-score (+ (first to-be-merged)
                                                                                        (second to-be-merged))))
                                                                (+ (first to-be-merged)
                                                                   (second to-be-merged)))))
                                       (recur (rest reverse-row-without-empty)
                                              (conj new-row (first to-be-merged)))))
                                   new-row)))]
    (into [] (concat (into [] (repeat (- (count row) (count merged-row))
                                      board-style))
                     (reverse merged-row)))))

(defn sort-board-row [row]
  (let [row-without-empty (sort (remove (fn [e] (if (= e board-style) true false)) row))]
    (into [] (concat (into [] (repeat (- (count row)
                                        (count row-without-empty))
                                     board-style))
                    row-without-empty))))

(defn sort-board-right [board]
  (into [] (map sort-board-row board)))

(defn sort-board-left [board]
  (into [] (map (fn [row] (vector-reverse (sort-board-row (vector-reverse row))))
                board)))

(defn sort-board-up [board]
  (transpose (sort-board-left (transpose board))))

(defn sort-board [board]
  (sort-board-up (sort-board-right board)))

(defn move-right [board]
  (into [] (map combine-row board)) )

(defn move-left [board]
  (into []  (map (fn [row] (vector-reverse (combine-row (vector-reverse row))))
                 board)))

(defn move-up [board]
  (transpose (move-left (transpose board))))

(defn move-down [board]
  (transpose (move-right (transpose board))))

(def KEY_UP 65)
(def KEY_DOWN 66)
(def KEY_LEFT 68)
(def KEY_RIGHT 67)
(def KEY_SORT 115)
(def way-action-map {KEY_LEFT move-left
                     KEY_RIGHT move-right
                     KEY_UP move-up
                     KEY_DOWN move-down
                     KEY_SORT sort-board})

(defn same-board? [old new]
  (= (reduce concat old) (reduce concat new)))

(defn move [board way-action]
  (let [old-board board
        new-board (way-action board)]
    [(same-board? old-board new-board) new-board]))

(defn next-way []
  (flush)
  (let [cr (ConsoleReader.)
        keyint (.readCharacter cr)
        way-action (way-action-map keyint)]
;    (println "keyint " keyint)
    (if way-action
      way-action
      (recur))))

;;* No room to add a new number
;;* Can not move
(defn can-move? [board way]
  (let [[not-changed -] (move board (way-action-map way))]
    (if not-changed
      false
      true)))

;;;;xxxx
(defn check-dead? [board]
  (if-not (random-row-with-empty board)
    (if (= (.indexOf (map (fn [way] (can-move? board way) )
                              [KEY_UP KEY_DOWN KEY_LEFT KEY_RIGHT])
                     true)
           -1)
      true
      false)
    false))

(defn dead? [board]
  (let [_ (set-fake-move true)]
    (let [is-dead? (check-dead? board)]
      (set-fake-move false)
      is-dead?)))

(defn start [saved-board]
  (loop [board saved-board]
    (do  (view/refresh-board board (get-total-score))
         (if (dead? board)
           (view/show-dead)
           (let [[not-changed board] (move board (next-way))]
             (if not-changed
               (recur board)
               (recur (new-num board))))))))

(defn board-next [board way]
  (do (view/refresh-board board (get-total-score))
      (if-not (dead? board)
        (let [[not-changed board] (move board way)]
          (if not-changed
            board
            (new-num board))))))

(defn start-multiboard [& boards]
  (loop [boards boards]
    (let [new-boards  (map (fn [board] (do (view/refresh-board board)
                                          (board-next board (next-way))))
                           boards)])))

(defn -main
  [& args]
  (println "Start... ")
  (let [[opts args banner] (cli args)]
    (let [[w h] args]
      (if (>= (count args) 2)
        (start (new-num (new-board (Integer. w) (Integer. h))))
        (start (new-num (new-board 4 4)))))))
