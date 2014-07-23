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

(declare extraction-list)

(defn edits-to-error-codes
  ([edits t1 t2] (edits-to-error-codes edits t1 t2 extraction-list))
  ([edits t1 t2 extraction-list]
     (let [{:keys [substitutions deletions insertions]} edits]
       (-> (reduce (fn [[codes edits] f]
                     (let [[ncodes nedits] (f edits t1 t2)]
                       [(concat codes ncodes) nedits])) [[] edits] extraction-list)
           first))))


(defn error-codes
  [t1 t2]
  (as-> (edits t1 t2) x
        (edits-to-error-codes x t1 t2)
        ;;sort the codes so that the error-codes are ordered by position in text
        (sort-by (comp second second) x)))

(defn to-code-number [^Character c]
  (cond
   (Character/isLetter c) 1
   (#{\. \, \? \!} c) 2
   (Character/isDigit c) 3
   (= c \space) 5
   :else 4))

(defn extract-substitution-errors [edits t1 t2]
  (let [codes (for [[p1 p2] (:substitutions edits)]
                (let [c1 (nth t1 p1) c2 (nth t2 p2)
                      [f1 f2] (map to-code-number [c1 c2])]
                  [[f1 f2] [p1 p2]]))]
    [codes (assoc edits :substitutions [])]))

(defn extract-insertion-errors [edits t1 t2]
  (let [codes (for [[p1 p2] (:insertions edits)]
                (let [ c2 (nth t2 p2)
                      f2 (to-code-number c2)]
                  [[8 f2] [p1 p2]]))]
    [codes (assoc edits :insertions [])]))

(defn extract-deletion-errors [edits t1 t2]
  (let [codes (for [[p1 p2] (:deletions edits)]
                (let [c1 (nth t1 p1) 
                      f1 (to-code-number c1)]
                  [[f1 8] [p1 p2]]))]
    [codes (assoc edits :deletions [])]))

(def bar-map
  {\A 2 \a 2 \B 2 \b 2 \C 2 \c 1 \D 2 \d 2 \E 2 \e 1 \F 2 \f 1 \G 3 \g 2
   \H 2 \h 2 \I 2 \J 2 \i 1 \j 1 \K 2 \k 1 \L 2 \l 1 \M 4 \m 3 \N 3 \n 2
   \O 2 \o 2 \P 3 \p 2 \Q 2 \q 2 \R 3 \r 1 \S 2 \s 2 \T 2 \t 1 \U 2 \u 2
   \V 3 \v 2 \W 4 \w 3 \X 2 \x 1 \Y 3 \y 2 \Z 2 \z 1 \ü 2})


(defn lookup-bar [char]
  (get bar-map char 1))

(defn- extract-following-substitutions [substitutions]
  (when (seq substitutions)
    (reduce (fn [l [a b]]
              (if (= (map inc (last (last l))) [a b])
                ;;add to current group
                (update-in l [(dec (count l))] #(conj % [a b]))
                ;;make a new group
                (conj l [[a b]])))
            [[(first substitutions)]] (rest substitutions))))

(defn- following [[a b] insdel type]
  (loop [[ia ib] [(inc a) (inc b)] acc []]
    (if (some #{[ia ib]} insdel)
      (recur (case type
               :one-to-many [ia (inc ib)]
               :many-to-one [(inc ia) ib]) (conj acc [ia ib]))
      acc)))

(defn- add-following [following-substitutions insdel type]
  (for [fs following-substitutions
        :let [if (following (last fs) insdel type)]
        :when (seq if)]
    [fs (concat fs if)]))

(defn- take-bars [t1 t2 [a b] insdel type]
  (let [res (let [bar-count (lookup-bar (case type
                                :one-to-many (nth t1 a)
                                :many-to-one (nth t2 b)))
                  _ (prn "bar-count " bar-count (nth t1 a))]
    (loop [[[ia ib] & is] insdel acc 0 ret []]
      (if ia
        (let [bc (lookup-bar (case type
                               :one-to-many (nth t2 ib)
                               :many-to-one (nth t1 ia)))
#_#_              _ (prn "bc " bc (nth t2 ib)
                     "acc " acc " ret " ret
                     (>= (+ acc bc) bar-count)
                     "ia ib " ia ib)]
          (if (>= (+ acc bc) bar-count)
            (conj ret [ia ib])
            (recur is (+ acc bc) (conj ret [ia ib]))))
        ret)))]
     (prn "take-bars " res)
     res))
  ;;;todo take care of how many are allowed to be extracted
  ;;;we know that count-free is > 0 at the beginning
  ;;;be careful at the start of the error
(defn- extract [t1 t2 fl type]
  (for [[fs insdel] fl]
    (let [_ (prn "fs " fs "insdel " insdel "count-free" (- (count insdel) (count fs)))
          count-free (- (count insdel) (count fs))]
      (loop [[[a b] & ss :as aktfs] fs extr [] insdel insdel
             count-free (- (count insdel) (count fs))]
        (if (and a (seq insdel))
          (let [ext (take-bars t1 t2 [a b] (take (inc count-free) insdel) type)
                _ (prn "ext-after-take-bars " [a b]  ext "insdel-for-ext " insdel "count-free " count-free)]
            (recur ;dont drop here
             ss
             (conj extr [[a b] ext])
             (drop (count ext) insdel)
             (- count-free (- (count ext) 1))))
          extr)))))

(defn- delete-from-edits [edits to-delete]
  (into {} (for [[k v] (dissoc edits :distance)] [k (remove (set to-delete) v)])))


(defn to-single-error [t1 t2 a b]
  (cond
   (= (count t1) a) (let [c2 (nth t2 b)
                          f2 (to-code-number c2)]
                       [[8 f2] [a b]])
   (= (count t2) b) (let [c1 (nth t1 a) 
                          f1 (to-code-number c1)]
                       [[f1 8] [a b]]);;deletion
   :else [[(to-code-number (nth t1 a))
           (to-code-number (nth t2 b))]
          [a b]]))

(defn extract-count-changing-errors [type edits t1 t2]
  (let [{:keys [substitutions insertions deletions]} edits
        fs (extract-following-substitutions substitutions)
        _ (prn "fs " fs)
        fi (add-following fs (case type :one-to-many insertions :many-to-one deletions) type)
        _ (prn "fi " fi)
        ext (apply concat (extract t1 t2 fi type))
        _ (prn "ext " ext)
        nedits (delete-from-edits edits (partition 2 (flatten ext)))
        res
        [(for [[[a b] e] ext]
           (case (count e)
             1 (let [[_ b] (first e)]
                 (to-single-error t1 t2 a b))
             (case type
               :one-to-many [[(to-code-number (nth t1 a)) 7]
                             [a (second (first e))] [(inc a) (second (last e))]]
               :many-to-one [[7 (to-code-number (nth t2 b))]
                             [(first (first e)) b] [(first (last e)) (inc b)]]))) nedits]
        _ (prn "extr " ext "res" (first res))]
    res))

(def extraction-list
  [(partial extract-count-changing-errors :one-to-many)
   (partial extract-count-changing-errors :many-to-one)
   extract-substitution-errors
   extract-insertion-errors
   extract-deletion-errors])

(use 'clojure.java.io)
(defn get-files-sorted [dir]
  (->> (file-seq (file dir))
       rest
       (sort-by #(.getName  %))))
#_(file-locations for now... "/home/kima/programming/ocr-visualizer/resources/public/ground-truth/" "/home/kima/programming/ocr-visualizer/resources/public/edits," "/home/kima/programming/ocr-visualizer/resources/public/ocr-results/")

(defn deploy-to-ocr-visualizer []
  (let [gts (get-files-sorted "/home/kima/programming/ocr-visualizer/resources/public/ground-truth/")
        ocr-res (get-files-sorted "/home/kima/programming/ocr-visualizer/resources/public/ocr-results/")]
    (doall (pmap (fn [gt ocr]
                   (let [filename (str "/home/kima/programming/ocr-visualizer/resources/public/edits/" (.getName gt))]
                     (prn "error-counts for " filename)
                     (spit filename (pr-str (error-codes (slurp gt) (slurp ocr))))))
                 gts ocr-res))))


(defn deploy-error-codes [base-directory]
  (let [gts (get-files-sorted (file base-directory "ground-truth/"))
        ocr-res (get-files-sorted (file base-directory "ocr-results/"))]
    (doall (map (fn [gt ocr]
                   (let [filename (file base-directory
                                        "edits/" (.getName gt))]
                     (prn "error-counts for " filename)
                     (spit filename (pr-str (error-codes (slurp gt) (slurp ocr))))))
                gts ocr-res))))

(defn word-count [text]
  (as-> text x
        (.split x "\\s*")
        (remove empty? x)
        (count x)))




(defn generate-statistics
  ([base-directory] (generate-statistics base-directory identity))
  ([base-directory flavour]
     (let [ocr-res (map slurp (get-files-sorted (file base-directory "ocr-results/")))
           edits (map (comp read-string slurp)  (get-files-sorted (file base-directory "edits/")))
           [errors charc] (reduce (fn [[edits wc] [nedits nocr]]
                                    [(concat edits nedits) (+ wc (count nocr))])
                                  ['() 0] (map (fn [a b] [a b]) edits ocr-res))]
       {:error-rate (* 100 (/ (count errors) charc))
        :charc charc :error-number (count errors)
        :by-category (into {} (map (fn [[k v]] [k (count v)]) (group-by first errors)))}
       )))



(defn visualize [error-code a b]
  (case (count error-code)
    2 ["Substitution from"
       (nth a (first (second error-code)))
       "to " (nth b (second (second error-code)))]
    3 (if (= 7 (second (first error-code)))
        ;;one-to-many
        ["One-to-many" (nth a (first (second error-code)))
         "to " (apply str (map #(nth b %) (range (second (second error-code))
                                                  (inc (second (nth error-code 2))))))]
        ["Dont know yet"])))
