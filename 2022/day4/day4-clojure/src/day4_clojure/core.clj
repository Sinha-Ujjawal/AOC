(ns day4-clojure.core
  (:gen-class)
  (:require [clojure.string :as string]))

(defn prompt [msg]
  (print msg)
  (flush)
  (read-line))

(defrecord Interval [^long start ^long end])

(defn parse-intervals-pair! [s]
  (let [matches (re-matches #"^(\d+)-(\d+),(\d+)-(\d+)$" s)]
    (if (nil? matches) (throw (Exception. (str "Cannot parse \"" s "\"")))
        [(Interval. (Long/parseLong (matches 1)) (Long/parseLong (matches 2)))
         (Interval. (Long/parseLong (matches 3)) (Long/parseLong (matches 4)))])))

(defn parse-intervals! [s]
  (->> (string/split-lines s)
       (map parse-intervals-pair!)))

(defn interval-completely-contains? [[{ll :start lh :end} {rl :start rh :end}]]
  (and
   (and (<= ll lh) (<= rl rh))
   (or
    (and (>= rl ll) (<= rh lh))
    (and (>= ll rl) (<= lh rh)))))

(defn solve-part-1 [interval-pairs]
  (->> interval-pairs
       (filter interval-completely-contains?)
       (count)))

(defn interval-overlaps? [[{ll :start lh :end} {rl :start rh :end}]]
  (and
   (and (<= ll lh) (<= rl rh))
   (or
    (and (<= ll rh) (<= rl lh))
    (and (<= rl lh) (<= ll rh)))))

(defn solve-part-2 [interval-pairs]
  (->> interval-pairs
       (filter interval-overlaps?)
       (count)))

(defn -main
  []
  (let [file-path (prompt "Enter file path: ")
        contents (slurp file-path)
        interval-pairs (parse-intervals! contents)]
    (println (str "Part 1: " (solve-part-1 interval-pairs)))
    (println (str "Part 2: " (solve-part-2 interval-pairs)))))
