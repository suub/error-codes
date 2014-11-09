(ns error-codes.core
  (:require [clojure.java.io :refer [file]]
            [clojure.set :as set]
            [taoensso.timbre :refer [debug]])
  (:use [clojure.core.matrix]))

(set! *warn-on-reflection* true)
(def ^:dynamic *DEBUG* false)

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
   (#{\ä \ö \ü \ß \Ä \Ö \Ü \( \) \„ \¬} c) 4
   (Character/isLetter c) 1
   (#{\. \, \? \! \: \;} c) 2
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
                  _ (debug "bar-count " bar-count (nth t1 a))]
    (loop [[[ia ib] & is] insdel acc 0 ret []]
      (if ia
        (let [bc (lookup-bar (case type
                               :one-to-many (nth t2 ib)
                               :many-to-one (nth t1 ia)))
#_#_              _ (debug "bc " bc (nth t2 ib)
                     "acc " acc " ret " ret
                     (>= (+ acc bc) bar-count)
                     "ia ib " ia ib)]
          (if (>= (+ acc bc) bar-count)
            (conj ret [ia ib])
            (recur is (+ acc bc) (conj ret [ia ib]))))
        ret)))]
     (debug "take-bars " res)
     res))
  ;;;todo take care of how many are allowed to be extracted
  ;;;we know that count-free is > 0 at the beginning
  ;;;be careful at the start of the error
(defn- extract [t1 t2 fl type]
  (for [[fs insdel] fl]
    (let [_ (debug "fs " fs "insdel " insdel "count-free" (- (count insdel) (count fs)))
          count-freepp (- (count insdel) (count fs))]
      (loop [[[a b] & ss :as aktfs] fs extr [] insdel insdel
             count-free (- (count insdel) (count fs))]
        (if (and a (seq insdel))
          (let [ext (take-bars t1 t2 [a b] (take (inc count-free) insdel) type)
                _ (debug "ext-after-take-bars " [a b]  ext "insdel-for-ext " insdel "count-free " count-free)]
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
        _ (debug "fs " fs)
        fi (add-following fs (case type :one-to-many insertions :many-to-one deletions) type)
        _ (debug "fi " fi)
        ext (apply concat (extract t1 t2 fi type))
        _ (debug "ext " ext)
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
        _ (debug "extr " ext "res" (first res))]
    res))

(def extraction-list
  [(partial extract-count-changing-errors :one-to-many)
   (partial extract-count-changing-errors :many-to-one)
   extract-substitution-errors
   extract-insertion-errors
   extract-deletion-errors])

(defn word-count [text]
  (as-> text x
        (.split x "\\s*")
        (remove empty? x)
        (count x)))


(defn error-code-to-matrix-entries [augmented-error-code]
  (let [[[[a b] & res :as error-code] [f t]] augmented-error-code]
    (cond
     (some #{8} [a b]) [[a b]]
     (= b 7) [[a b]]
     (= a 7) (map (fn [fc] [(to-code-number fc) 6]) f)
     (or (= (str f) (.toUpperCase (str t)))
         (= (str t) (.toLowerCase (str f))))
     [[1 9]]
     :else [[a b]])))
(defn save-nth [x p]
  (if (>= p (count x)) \?
      (nth x p)))

(defn save-subs [x ps pe]
  (if (or (>= ps (count x))
          (>= pe (count x)))
    "?" (subs x ps pe)))

(defn build-error [a b error-code]
  (cond
   (= 8 (get-in error-code [0 0]))
   ["" (str (save-nth b (get-in error-code [1 1])))]
   (= 8 (get-in error-code [0 1]))
   [(str (save-nth a (get-in error-code [1 0]))) ""]
   (= 2 (count error-code))
   (let [[code [l r]] error-code] ;;todo
     [(str (save-nth a l))
      (str (save-nth b r))])
   (= 7 (get-in error-code [0 1]))
   (let [[code [ls rs] [le re]] error-code]
     [(save-subs a ls le) (save-subs b rs (inc re))])
   (= 7 (get-in error-code [0 0]))
   (let [[code [ls rs] [le re]] error-code]
     [(save-subs a ls (inc le)) (save-subs b rs re)])))



(defn augment-error-code [a b error-code]
  [error-code (build-error a b error-code)])


(defn no-insertions-or-deletions [matrix-entry]
  (not (some #{8} matrix-entry)))

(def stichwortsuche #{[1 1] [1 2] [1 3] [1 4] [1 5] [1 6] [1 7] [1 8]
                      [3 1] [3 2] [3 3] [3 4] [3 5] [3 6] [3 7] [3 8]
                      [4 1] [4 2] [4 3] [4 4] [4 5] [4 6] [4 7] [4 8]
                      [8 1] [8 2] [8 3] [8 4] [8 5] [8 6] [8 7]})

(def case-sensitive-stickwortsuche
  (set/union stichwortsuche #{[1 9] [4 9]}))

(def phrasensuche
  (set/union stichwortsuche
             #{[5 1] [5 2] [5 3] [5 4] [5 5] [5 6] [5 7] [5 8]}))

(defn strip-first-and-last-lines [errors truth raw]
  (let [lines (.split truth "\n")
        end-of-first-line (count (first lines))
        start-of-last-line (- (count truth) (count (last lines)))]
    (debug end-of-first-line start-of-last-line)
    (filter (fn [[code [truth _] & rest]]
              (> start-of-last-line truth end-of-first-line)) errors)))


(defn strip-start-and-end-errors [errors truth raw]
  (let [starting (take-while #(some (fn [[code [_ raw] & rest]]
                                      (= % raw)) errors) (range))
        ending (take-while #(some (fn [[code [_ raw] & rest]]
                                    (= % raw)) errors)
                           (range (dec (count raw)) 0 -1))
        starting (or (last starting) 0)
        ending (or (last ending) (count raw))]
    (debug starting ending)
    (filter (fn [[code [_ raw] & rest]]
              (> ending raw starting)) errors)))

(defn strip-newline-errors [errors truth raw]
  (remove (fn [error]
            (->> error
                 (augment-error-code truth raw)
                 second
                 (some #(.contains %1 "\n")))) errors))


(def flavour-list
  {"stichwortsuche" [[strip-newline-errors strip-start-and-end-errors]
                     stichwortsuche]
   "case-sensitive-stichwortsuche" [[strip-newline-errors strip-start-and-end-errors] case-sensitive-stickwortsuche]
   "phrasensuche" [[strip-newline-errors strip-start-and-end-errors] phrasensuche]
   "alle Fehler" [[strip-newline-errors strip-start-and-end-errors]
                   (constantly true)]})

(defn run-filters [filters errors ground-truth raw-results]
  (reduce (fn [errors filter]
            (filter errors ground-truth raw-results))
          errors filters))

(defn generate-statistics ;;setze vereinbarte standartwerte
  ([pages edit-sel] (generate-statistics pages edit-sel identity))
  ([pages edit-sel flavour]
     (let [errors (->> (mapcat (fn [{:keys [truth raw edits]}]
       (map (partial augment-error-code truth raw) edit-sel edits)) pages)
                       (mapcat error-code-to-matrix-entries)
                       (filter flavour))
           charc (count (mapcat :raw pages))]
       {:error-rate (double (* 100 (/ (count errors) charc)))
        :charc charc :error-number (count errors)
        :by-category (frequencies errors)})))

(defn correction-statistic [{:keys [truth raw corrected edits]}]
  ;;todo wenn korrektur zeichen löscht oder einfügt dann ist difference hier nicht mehr korrekt
  ;;es müsste dann gefiltert werden wobei die position im ground truth als kennzeichnung dient
  (let [{:keys [truth-raw truth-corrected]} edits
        false-positives (set/difference (into #{} truth-corrected) (into #{} truth-raw))
        false-negatives (set/difference  (into #{} truth-corrected) false-positives)
        true-positives (set/difference (into #{} truth-raw) (into #{} truth-corrected))]
    {:false-positives (map (partial augment-error-code truth corrected) false-positives)
     :false-negatives (map (partial augment-error-code truth corrected) false-negatives)
     :true-positives (map (partial augment-error-code truth raw) true-positives )}))
