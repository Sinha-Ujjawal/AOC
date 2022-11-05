(ns day6-clojure.core
  (:gen-class)
  (:require [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const grid-size 1000)

(defn init-light-grid []
  (long-array (* grid-size grid-size)))

(defn update-grid! [update-fn
                    [^long rowl ^long coll ^long rowu ^long colu]
                    ^longs grid]
  (doseq [^long row (range rowl (inc rowu))
          ^long col (range coll (inc colu))]
    (let [idx (+ (* grid-size row) col)]
      (aset grid idx (Math/max 0 ^long (update-fn ^long (aget grid idx)))))))

(defn apply-instruction! [turn-on-update-fn
                          turn-off-update-fn
                          toggle-update-fn
                          ^longs grid
                          [instruction-type box]]
  (cond
    (= instruction-type :turn-on) (update-grid! turn-on-update-fn box grid)
    (= instruction-type :turn-off) (update-grid! turn-off-update-fn box grid)
    (= instruction-type :toggle) (update-grid! toggle-update-fn box grid)
    :else grid))

(defn solve [turn-on-update-fn!
             turn-off-update-fn!
             toggle-update-fn!
             instructions]
  (let [^longs grid (init-light-grid)]
    (doseq [instruction instructions]
      (apply-instruction! turn-on-update-fn!
                          turn-off-update-fn!
                          toggle-update-fn!
                          grid
                          instruction))
    grid))

(defn count-positives [^longs arr]
  (areduce arr i ret (long 0)
           (if (> (aget arr i) 0) (inc ret) ret)))

(defn solve-part-1 [instructions]
  (let [turn-on-update-fn (fn [_] 1)
        turn-off-update-fn (fn [_] 0)
        toggle-update-fn (fn [^long b] (if (zero? b) 1 0))]
    (->> instructions
         (solve turn-on-update-fn turn-off-update-fn toggle-update-fn)
         (count-positives))))

(defn sum-positives [^longs arr]
  (areduce arr i ret (long 0)
           (let [val (aget arr i)]
             (if (> val 0) (+ ret val) ret))))

(defn solve-part-2 [instructions]
  (let [turn-on-update-fn inc
        turn-off-update-fn dec
        toggle-update-fn (fn [^long x] (+ x 2))]
    (->> instructions
         (solve turn-on-update-fn turn-off-update-fn toggle-update-fn)
         (sum-positives))))

(defn parse-turn-on [line]
  (let [xs (rest (re-find #"turn on (\d+)\,(\d+) through (\d+)\,(\d+)" line))]
    (if (= 4 (count xs))
      [:turn-on (mapv #(Long/parseLong %) xs)]
      nil)))

(defn parse-turn-off [line]
  (let [xs (rest (re-find #"turn off (\d+)\,(\d+) through (\d+)\,(\d+)" line))]
    (if (= 4 (count xs))
      [:turn-off (mapv #(Long/parseLong %) xs)]
      nil)))

(defn parse-toggle [line]
  (let [xs (rest (re-find #"toggle (\d+)\,(\d+) through (\d+)\,(\d+)" line))]
    (if (= 4 (count xs))
      [:toggle (mapv #(Long/parseLong %) xs)]
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
  ([] (-main (prompt "Enter file name: ")))
  ([file-name & _]
   (let [instructions (parse-from-file file-name)
         part-1-ans (solve-part-1 instructions)
         part-2-ans (solve-part-2 instructions)]
     (println "Part 1: " part-1-ans)
     (println "Part 2: " part-2-ans))))
