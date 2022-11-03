(ns day6-clojure.core
  (:gen-class)
  (:require [clojure.string :as string]))

(def init-light-grid
  (vec (repeat (* 1000 1000) 0)))

(defn box-coords [[rowl coll rowu colu]]
  (for [row (range rowl (inc rowu))
        col (range coll (inc colu))
        :let [idx (+ (* row 1000) col)]]
    idx))

(defn update-grid [update-fn box grid]
  (let [reducer (fn [grid idx] (assoc! grid idx (max 0 (update-fn (grid idx)))))]
    (reduce reducer grid (box-coords box))))

(defn apply-instruction [turn-on-update-fn
                         turn-off-update-fn
                         toggle-update-fn
                         grid [instruction-type box]]
  (cond
    (= instruction-type :turn-on) (update-grid turn-on-update-fn box grid)
    (= instruction-type :turn-off) (update-grid turn-off-update-fn box grid)
    (= instruction-type :toggle) (update-grid toggle-update-fn box grid)
    :else grid))

(defn solve [turn-on-update-fn
             turn-off-update-fn
             toggle-update-fn
             instructions]
  (let [reducer (partial apply-instruction turn-on-update-fn turn-off-update-fn toggle-update-fn)]
    (persistent! (reduce reducer (transient init-light-grid) instructions))))

(defn solve-part-1 [instructions]
  (let [turn-on-update-fn (fn [_] 1)
        turn-off-update-fn (fn [_] 0)
        toggle-update-fn (fn [b] (bit-xor b 1))]
    (->> instructions
         (solve turn-on-update-fn turn-off-update-fn toggle-update-fn)
         (filter #(> % 0))
         (count))))

(defn solve-part-2 [instructions]
  (let [turn-on-update-fn inc
        turn-off-update-fn dec
        toggle-update-fn #(+ 2 %)]
    (->> instructions
         (solve turn-on-update-fn turn-off-update-fn toggle-update-fn)
         (filter #(> % 0))
         (apply +))))

(defn parse-turn-on [line]
  (let [xs (rest (re-find #"turn on (\d+)\,(\d+) through (\d+)\,(\d+)" line))]
    (if (= 4 (count xs))
      [:turn-on (mapv #(Integer/parseInt %) xs)]
      nil)))

(defn parse-turn-off [line]
  (let [xs (rest (re-find #"turn off (\d+)\,(\d+) through (\d+)\,(\d+)" line))]
    (if (= 4 (count xs))
      [:turn-off (mapv #(Integer/parseInt %) xs)]
      nil)))

(defn parse-toggle [line]
  (let [xs (rest (re-find #"toggle (\d+)\,(\d+) through (\d+)\,(\d+)" line))]
    (if (= 4 (count xs))
      [:toggle (mapv #(Integer/parseInt %) xs)]
      nil)))

(defn parse-line [line]
  (or (parse-turn-on line)
      (parse-turn-off line)
      (parse-toggle line)))

(defn parse-lines [lines]
  (remove nil? (mapv parse-line lines)))

(defn parse-from-file [file]
  (-> (slurp file)
      (string/split #"\n")
      (parse-lines)))

(defn prompt [msg]
  (print msg)
  (flush)
  (read-line))

(defn -main
  [& _]
  (let [file-name (prompt "Enter file name: ")
        instructions (parse-from-file file-name)
        part-1-ans (solve-part-1 instructions)
        part-2-ans (solve-part-2 instructions)]
    (println "Part 1: " part-1-ans)
    (println "Part 2: " part-2-ans)))
