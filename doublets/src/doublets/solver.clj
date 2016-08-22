(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn count-different-letters [word1 word2]
  (->> (map = word1 word2)
       (filter false?)
       (count)))

(defn is-one-letter-off? [word1 word2]
  (and
    (= 1 (count-different-letters word1 word2))
    (= (count word1) (count word2))))

(defn find-valid-next-words [wordlist]
  (let [new-words (filter #(is-one-letter-off? (last wordlist) %) words)]
    (filter #(not (contains? (into #{} wordlist) %)) new-words)))

(defn children [wordlist]
  (->> wordlist
       find-valid-next-words
       (map #(conj wordlist %))))

(defn find-shortest-path [paths]
  (if (> (count paths) 0)
    (->> paths
         (sort-by count)
         (first))
    paths))

(defn doublets [word1 word2]
  (let [possibilities (tree-seq (constantly true) #(children %) [word1])]
    (->> possibilities
         (filter #(= (last %) word2))
         find-shortest-path)))