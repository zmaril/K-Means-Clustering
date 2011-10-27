(ns KMeans.core
  (:use [clojure.contrib.math]
        [clojure.contrib.string :only (split)]
))

;;Where my files be at
(def file-name "/Users/zackmaril/Dropbox/clojure/KMeans/src/KMeans/traffic_patterns.txt" )

;;Little bit of magic to read in the file and get it into a useful map
(def all-compys
  ;;Let binding to bring all this stuff in
  (let [lines   (with-open [rdr (clojure.java.io/reader file-name)]
                  (reduce conj [] (line-seq rdr)))
        ;;Slipt everything up by \t's
        split-lines (map (partial split #"\t") lines)]
    ;;Create the list of compaines to work with.
    (map #(assoc {}:name (first %)
            :timings (map (fn [n] (Float/parseFloat n)) (rest %)))
         split-lines)))

;;Use only the dailies with the filter
(def dailies (filter #(> 30 (count (:timings %))) all-compys))

;;Use only the weeklies with the filter
(def weeklies (filter #(< 30 (count (:timings %))) all-compys))

;;Average of a list of numbers
(defn average [lst]
  (/ (reduce + lst) (count lst)))

;;Deviation of a list of numbrs
(defn deviation [lst]
  (let [mu (average lst)] 
    (sqrt (average (map #(expt (- % mu) 2) lst)))))

;;Normalize a value given mu and sigma
(defn normalize [value mu sigma]
  (/(- value mu) sigma))

;;Normalizes a freq
(defn correlate-freq [freq]
  (let [mu (average freq)
        sigma (deviation freq)]
    (map #(normalize % mu sigma) freq)))

;;Computes the temporal correlation of two freq's
(defn temp-correlation [freq1 freq2]
  (let [corr1 (correlate-freq freq1)
        corr2 (correlate-freq freq2)]
    (/ (reduce + (map * corr1 corr2)) (count freq1))))

;; Comparitive function to find which in the set of centroids are
;; closest to a certain value. 
(defn closest-by-time [centriods value]
  (reduce #(if  (> (temp-correlation %1 (:timings value))
                   (temp-correlation %2 (:timings value)))
             %1 %2)
          centriods))

;; Holla http://erl.nfshost.com/2011/05/18/the-transpose-function/
(defn transpose [xs]
  (apply map vector xs))

;;Takes the samleps, a measure, and the centroids, and produces the
;;next set of centroids. 
(defn iterate-centroids [samples measure centroids]
  (let [clusters (group-by (partial measure centroids) samples)]
    (map (fn [cluster]
           (map average (transpose (map :timings (nth cluster 1))))) clusters)))

;;Main algorithm, sets everything up and then runs it. Note that k
;;means is mostly just one function that takes in the centroids and
;;outputs the next ones within the enviroment of a certain measure and
;;some parameters. 
(defn k-means [number-clusters samples]
  (let [values (map :timings samples)
        initial-centroids (take number-clusters
                                ;;1/n 0,0,0 centroid
                                (repeatedly (fn [] (map #(average  [%1 %2])
                                                        (rand-nth values)
                                                        (rand-nth values)))))]
    (nth (iterate (partial iterate-centroids samples  closest-by-time) initial-centroids) 20) ))

;;Group everything into the correct clusters. Uses the group-by
;;function to do this. 
(def clusters (group-by (partial closest-by-time (k-means 15 dailies)) dailies))

;; Gets all the names and goes to town. 
(def names (map #(map :name (nth % 1)) clusters))
