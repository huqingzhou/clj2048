(ns clj2048.view
    (:gen-class))

(defn refresh-row [row]
  (if (seq row)
    (do (print (format "%4s " (str(first row))))
        (recur (rest row)))))

(defn clear-screen []
  (dotimes [n 2000] (println "")))

(defn refresh-board [board score]
  (clear-screen)
  (println (format  "<Hint: press 's' to sort the board>"))
  (println "")
  (println (format  "SCORE: %5d " score))
  (println "")
  (println "")
  (loop [b board]
    (if (seq b)
      (do (refresh-row (first b))
          (println "")
          (println "")
          (recur (rest b))))))

(defn show-dead []
  (println "Over ^-^ Bye"))
