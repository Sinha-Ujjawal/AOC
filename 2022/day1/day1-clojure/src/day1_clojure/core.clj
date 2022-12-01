(ns day1-clojure.core
  (:gen-class)
  (:require [clojure.string :as string]))

(defn prompt [msg]
  (print msg)
  (flush)
  (read-line))

(defn parse-elf-calries-from-string [s]
  (->> (string/split s #"\n")
       (map #(Integer/parseInt %))))

(defn parse-elves-calories-from-string [s]
  (->> (string/split s #"\n\n")
       (map parse-elf-calries-from-string)))

(defn parse-elves-calories-from-file [path]
  (-> (slurp path)
      (parse-elves-calories-from-string)))

(defn solve-part-1 [elves-calories]
  (->> elves-calories
       (map #(apply + %))
       (apply max)))

(defn solve-part-2 [elves-calories]
  (->> elves-calories
       (map #(apply + %))
       (sort)
       (reverse)
       (take 3)
       (apply +)))

(defn -main
  []
  (let [file-name (prompt "Enter file path: ")
        elves-calories (parse-elves-calories-from-file file-name)
        part-1-ans (solve-part-1 elves-calories)
        part-2-ans (solve-part-2 elves-calories)]
    (println "Part 1: " part-1-ans)
    (println "Part 2: " part-2-ans)))
