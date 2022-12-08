(ns day3-clojure.core
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn prompt [msg]
  (print msg)
  (flush)
  (read-line))

(defn priority-char [c]
  (if-not (Character/isAlphabetic (int c))
    nil
    (if (Character/isLowerCase (int c))
      (inc (- (int c) (int \a)))
      (+ 27 (- (int c) (int \A))))))

(defn priority-badge [badge]
  (transduce (map priority-char) + badge))

(defn badge-rusk-sacks [rusk-sacks]
  (apply set/intersection (map set rusk-sacks)))

(defn priority-rusk-sacks [rusk-sacks]
  (priority-badge (badge-rusk-sacks rusk-sacks)))

(defn solve [n rusk-sacks]
  (->> rusk-sacks
       (partition n)
       (transduce (map priority-rusk-sacks) +)))

(defn split-half [s]
  (let [n (count s)
        half-n (/ n 2)]
    [(take half-n s) (drop half-n s)]))

(defn solve-part-1 [s]
  (->> (string/trim s)
       (string/split-lines)
       (mapcat split-half)
       (solve 2)))

(defn solve-part-2 [s]
  (->> (string/trim s)
       (string/split-lines)
       (solve 3)))

(defn -main
  []
  (let [file-path (prompt "Enter file path: ")
        contents (slurp file-path)]
    (println (str "Part 1: " (solve-part-1 contents)))
    (println (str "Part 2: " (solve-part-2 contents)))))
