(ns error-codes.core
  (:require [clojure.java.io :refer [file]])
  (:use [clojure.core.matrix]))

(set! *warn-on-reflection* true)

(defn lev [str1 str2]
  (let [mat (new-matrix :ndarray (inc (count str1)) (inc (count str2)))]
     (mset! mat 0 0 0)
     (dotimes [i (count str1)]
       (mset! mat (inc i) 0 (inc i)))
     (dotimes [j (count str2)]
       (mset! mat 0 (inc j) (inc j)))
     (dotimes [dj (count str2)]
       (dotimes [di (count str1)]
         (let [j (inc dj) i (inc di)]
           (mset! mat i j
                  (cond
                   (= (.charAt ^String str1 di) (.charAt ^String str2 dj))
                   (mget mat di dj)
                   :else
                   (min (inc (mget mat di j)) (inc (mget mat i dj))
                        (inc (mget mat di dj))))))))
     mat))

(defn backtrace [d i j acc]
  (cond
   (and (> i 0) (= (inc (mget d (dec i) j)) (mget d i j)))
   (recur d (dec i) j (assoc acc :deletions (cons [(dec i) j] (:deletions acc))))
   (and (> j 0) (= (inc (mget d i (dec j))) (mget d i j)))
   (recur d i (dec j) (assoc acc :insertions (cons [i (dec j)] (:insertions acc))))
   (and (> i 0) (> j 0) (= (inc (mget d (dec i) (dec j))) (mget d i j)))
   (recur d (dec i) (dec j) (assoc acc :substitutions (cons [(dec i) (dec j)] (:substitutions acc))))
   (and (> i 0) (> j 0) (= (mget d (dec i) (dec j)) (mget d i j)))
   (recur d (dec i) (dec j) acc)
   :else acc))

(defn edits [a b]
  (let [d (lev a b)]
    (backtrace d (count a) (count b) {:insertions '() :deletions '()
                                      :substitutions '()
                                      :distance (mget d (count a) (count b))})))
