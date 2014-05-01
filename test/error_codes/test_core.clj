(ns error-codes.test-core
  (:require [clojure.test :refer :all]
            [error-codes.core :refer :all]
            [clojure.core.matrix :refer :all]))

(defn lev-distance [a b]
   (let [m (lev a b)]
     (mget m (count a) (count b))))
 
 (deftest test-lev
   (is (= 1 (lev-distance "" "a")))
   (is (= 1 (lev-distance "a" "")))
   (is (= 3 (lev-distance "kitten" "sitting"))))

(test-lev)

(clojure.pprint/pprint (do (deftest test-edits
  (is (= (edits "a" "b") 
         '{:insertions (), :deletions (), :substitutions ([0 0]), :distance 1}))
  ;;swapping two characters is not multiple substitutions but insertion and deletions
  ;;which is more in line with what humans see there.
  (is (= (edits "ab" "ba")
         '{:insertions ([0 0]), :deletions ([1 2]), :substitutions (), :distance 2}))
  (is (= (edits "vr" "io")
         '{:insertions (), :deletions (), :substitutions ([0 0] [1 1]), :distance 2}))
  ;;many to one errors are substitutions followed by insertions
  (is (= (edits "m" "rn")
         '{:insertions ([1 1]), :deletions (), :substitutions ([0 0]), :distance 2}))
  ;;one to many errors are substitutions followed by deletions
  (is (= (edits "rn" "m")
         '{:insertions (), :deletions ([1 1]), :substitutions ([0 0]), :distance 2}))
  (is (= (edits "Kitten" "sitting")
         '{:insertions ([6 6]), :deletions (), :substitutions ([0 0] [4 4]), :distance 3}))
  (is (= (edits "Kitten" "sittieng")
         '{:insertions ([4 4] [6 7]), :deletions (), :substitutions ([0 0]), :distance 3}))
  (is (= (edits "Kitten" "iiittiing")
         '{:insertions ([2 2] [5 6] [6 8]), :deletions (), :substitutions ([0 0] [4 5]), :distance 5}))
  )


(test-edits)))
