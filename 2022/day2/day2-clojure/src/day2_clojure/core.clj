(ns day2-clojure.core
  (:gen-class)
  (:require [clojure.string :as string]))

(defn prompt [msg]
  (print msg)
  (flush)
  (read-line))

(defrecord Play [opponent, you])

(defn ->outcome [play]
  (condp = [(:you play) (:opponent play)]
    [:rock :scissor] :won
    [:scissor :paper] :won
    [:paper :rock] :won
    (if (= (:you play) (:opponent play)) :tie :lost)))

(defn decide [hand outcome]
  (condp = [hand outcome]
    [:rock :lost] :scissor
    [:paper :lost] :rock
    [:scissor :lost] :paper
    [:rock :won] :paper
    [:paper :won] :scissor
    [:scissor :won] :rock
    hand))

(defn score-hand [hand]
  (condp = hand
    :rock 1
    :paper 2
    :scissor 3))

(defn score-outcome [outcome]
  (condp = outcome
    :won 6
    :tie 3
    :lost 0))

(defn score-play [play]
  (+ (score-hand (:you play)) (score-outcome (->outcome play))))

(defn score-plays [plays]
  (transduce (map score-play) + plays))

(defn parse-line-1 [s]
  (condp = s
    "A X" (Play. :rock :rock)
    "A Y" (Play. :rock :paper)
    "A Z" (Play. :rock :scissor)
    "B X" (Play. :paper :rock)
    "B Y" (Play. :paper :paper)
    "B Z" (Play. :paper :scissor)
    "C X" (Play. :scissor :rock)
    "C Y" (Play. :scissor :paper)
    "C Z" (Play. :scissor :scissor)))

(defn parse-plays-1 [s]
  (->> (string/trim s)
       (string/split-lines)
       (map parse-line-1)))

(defn solve-part-1 [s]
  (score-plays (parse-plays-1 s)))

(defn parse-line-2 [s]
  (condp = s
    "A X" (Play. :rock (decide :rock :lost))
    "A Y" (Play. :rock (decide :rock :tie))
    "A Z" (Play. :rock (decide :rock :won))
    "B X" (Play. :paper (decide :paper :lost))
    "B Y" (Play. :paper (decide :paper :tie))
    "B Z" (Play. :paper (decide :paper :won))
    "C X" (Play. :scissor (decide :scissor :lost))
    "C Y" (Play. :scissor (decide :scissor :tie))
    "C Z" (Play. :scissor (decide :scissor :won))))

(defn parse-plays-2 [s]
  (->> (string/trim s)
       (string/split-lines)
       (map parse-line-2)))

(defn solve-part-2 [s]
  (score-plays (parse-plays-2 s)))

(defn -main
  []
  (let [file-path (prompt "Enter file path: ")
        contents (slurp file-path)]
    (println (str "Part 1: " (solve-part-1 contents)))
    (println (str "Part 2: " (solve-part-2 contents)))))
