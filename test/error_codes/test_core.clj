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

(deftest test-error-codes
  (is (= (error-codes "a" "b")
         '([[1 1] [0 0]])))
  (is (= (error-codes "kitten" "sitting")
         '([[1 1] [0 0]] [[1 1] [4 4]] [[8 1] [6 6]])))
  (is (= (error-codes "m" "rn")
         '([[1 7] [0 0] [1 1]])))
  (is (= (error-codes "Mammut" "rnarniiiut")
         '([[1 7] [0 0] [1 1]] [[1 7] [2 3] [3 4]] [[1 7] [3 4] [4 7]])))
  (is (= (error-codes "rnarniiiut" "Mammut")
         '([[7 1] [0 0] [1 1]] [[7 1] [3 2] [4 3]] [[7 1] [4 3] [7 4]]))))

(test-error-codes)

(deftest big-error-codes-test
  (is (= (error-codes "78\n\nschneider, vom bloßen Geldverdienst absehend, mit schöner Beharrlichkeit dem Ziele\nnachstrebten, ein wirkliches Kunstwerk zu schaffen. Daß ihnen dies bis zu einem\ngewissen Grade gelungen ist, leidet keinen Zweifel; jedenfalls ist die Federzeichnung,\nnach welcher der Holzschneider gearbeitet hat, in allen Stücken getreu und correct,\nwird möchten sagen zu correct und getreu, wiedergegeben.\n\tDas Werk stellt eine Felsschlucht dar, in welcher eine Löwin ihren vor ihr\nhingestreckten, von einem Wurfspieß durchbohrten Gemahl betrauert, während oben\ndurch eine Oeffnung in der Steinwand Beduinenjäger sichtbar werden, die auch ihr\nLeben zu bedrohen scheinen. Die Gruppirung dieser Figuren ist gut, die Bewegung\nder Löwin ist - in der Conception - ebenfalls angemessen. Die Jäger hätten\nfüglich wegbleiben können, da sie, wofern sie den auch der Löwin drohenden Tod\nandeuten sollen, die eigentliche Wirkung des Bildes der trauernden Löwin stören;\nsollen sie aber sagen, daß der Löwe durch Jäger umgekommen ist, so sind sie über-\nflüssig, da die Ursache des Todes schon hinreichend durch den im Leibe des Thieres\nsteckenden abgebrochenen Spieß angegeben ist.\n\tDas Bild würde ferner an Wirkung gewonnen haben durch eine feinere Beob-\nachtung des Stofflichen. Das Fell der Thiere, Sandboden mit Halfehgras, Felswand,\nPalmen und Aloe sind in der technischen Behandlung jedenfalls zu gleichmäßig. So\nhätte beispielsweise die Schattenseite der Felswand, rechts wo die Jäger herablugen,\nruhiger und in zurückweichenden Tönen behandelt werden sollen. Der Körper des lie-\ngenden Löwen hätte sich mehr rund von der Fläche abheben müssen, wie auch die ganze\nMuskulatur der Thiere noch präciser und energischer sein könnte. Endlich aber will\ndas Blut vor dem Maule des todten Löwen uns nicht recht wie Blut erscheinen.\nTrotz dieser Ausstellungen an den Einzelnheiten verdient das Blatt als Ganzes -\nnamentlich als tüchtiger gesunder Holzschnitt - den besten Leistungen der Gegen-\nwart auf diesem Gebiet beigerechnet zu werden, und in dieser Eigenschaft empfehlen\nwir es allen Freunden der Kunst angelegentlich.\n\nLiteratur.\n\n\tDie Böhmischen Exulanten in Sachsen von Chr. A. Pescheck Leipzig,\nS. Hirzel 1857. - Es ist von mehrfachem Interesse zu ermitteln, wie die einzel-\nnen Bölterstämme des gegenwärtigen Deutschlands durch die Uebergänge der In-\ndividuen aus einem Stamme in den andern allmälig zu einer deutschen Nation\ngemischt worden sind. Das Ineinanderfließen der Stämme durch Ein- und Aus-\nwanderung war während fast zwei Jahrtausenden niemals ganz unterbrochen, hat\naber in verschiedenen Zeiträumen besondere Ausdehnung erreicht. Von der politischen\nGeschichte wird das massenhafte Einströmen der Deutschen in das Slavenland zwischen\nElbe und Weichsel noch am ausführlichsten behandelt. Aber nicht weniger eigenthümlich\nwaren die Verhältnisse in Böhmen. Seit dem frühsten Mittelalter fand dorthin ein fried-\nliches Einziehen deutscher Bildung und deutscher Individuen statt. Doch die deutsche\nColonisation des Landes wurde mehr als einmal durch eine kräftige Gegenströmung\n"


    " \n\n \n\n  \n\n78\n\nschneidet, vom bloßen Geldberdienst absehend, mit schöner Beharrlichkeit dem Ziele\nnachstrebten, ein wirtliches Kunstiverk zu schaffen. Daß ihnen dies bis zu einem\ngewissen Grade gelungen ist, leidet keinen Zweifel; jedenfalls ist die Federzeiehnung\nnach welcher der Holzschneidcr gearbeitet hat, in allen Stücken getreu und eorrect,\nwird möchten sagen zu eorrect und getreu, niiedkkgegelbkk\n\nDas Werk stellt eine Felsschlucht dar, in welcher eine Löwin ihren vor ihr\nlniigestrecktenj von einein Wurfspieß durchbohrter! Gemahl betrauert, während oben\ndurch eine Oeffnung in der Steinwand Beduiiienjägssr sichtbar werden, die auch ihr\nLeben zu bedrohen scheinen. Die Gruppiruiig dieser Figuren ist gut, die Bewegung\nder Löwin ist —-- in der Conceptivii —-— ebenfalls angemessen. Die Jäger hätten\nfüglich wegbleiben können, da sie, wofern sie den auch der Löwin drohenden Tod\nandeuten sollen, die eigentliche Wirkung des Bildes der trauernden Löwin stören;\nsollen sie aber sagen, daß der Löwe durch Jäger umgekommen ist, so smd sie Liber-\nflüssig, da die Ursache des Todes schon hinreichend durch den im Leibe des Thieres\nsteckenden abgebrochenen Spieß angegeben ist.\n\nDas Bild wiirde ferner an Wirkung gewonnen haben durch eine feinere Beob-\nachtung des Stofflichen. Das Fell der Thiere, Sandboden mit Halfehgras Felswand,\nPalmen undAloe sind in der technischen Behandlung jedenfalls zu gleichniäßig So\nhätte beispielsweise die Schattenseite der Felswand, rechts wo die Jäger herablugen,\nruhiger und in zuriietkveichendeii Tönen behandelt werden sollen. Der Körper des lie-\ngenden Löwen hätte sich mehr rund von der Fläche abhebeii müssen, wie auch die ganze\nMuskulatur derThiere noch präciser und energischer sein könnte. Endlich aber will\ndas Blut vor den! Maule des todten Löwen uns nicht recht wie Blut erscheinen.\nTrotz dieser Ansstellungen an den Einzelnhciten verdient das Blatt als Ganzes —-\nnamentlich als tüchtiger gesunder Holzsehnitt — den besten Leistungen der Gegen-\n\n \n\nwart auf diescni Gebiet beigereehnet zu werden, und in dieser Eigenschaft empfehlen.\n\nwir es allen Freunden der Kunst angelegentlichJ «\n\nLiteratur.\n\nDie Böhmischen Exulanten in Sachsen von Chr. A. Pescheclc Leipzig- «\n\nS. Hirzel 1857. —- Es ist von mehrfacheni Interesse zu ermitteln, wie die einzel-\nnen Bölterstänune des gegenwärtigen Deutschlands durch die Uebergäiige der IN-\ndividuen aus einem Stamme in den andern allmälig zu einer deutschen Nation\ngemischt worden sind Das Ineinanderfließen der Stämme durch Em- und Aus-\nwanderung war während fast zwei Jahrtausenden niemals ganz unterbrochen, hat\naber in verschiedenen Zeitriiumen besondere Ausdehnung erreicht. Von der politischen\nGeschichte wird das massenhafteEinströiiieii der Deutschen in das Slavenland zwischen\nElbe Und Weichsel noch am ausführlichsteii behandelt. Aber nicht weniger eigenthiinilich\nwaren die Verhältnisse in Böhmen. Seit dem friihsten Neittelalter fand dorthin ein fried-\nliches Einziehen deutscher Bildung und deutscher Individuen statt. Doch die dcutsche\nColonisation des Landes wurde niehr als einmal durch eine triistige lihegeiiströmung\n\n  \n\n")
         '([[8 5] [0 0]] [[8 4] [0 1]] [[8 4] [0 2]] [[8 5] [0 3]] [[8 4] [0 4]] [[8 4] [0 5]] [[8 5] [0 6]] [[8 5] [0 7]] [[8 4] [0 8]] [[8 4] [0 9]] [[1 1] [12 22]] [[1 1] [30 40]] [[1 1] [108 118]] [[1 7] [121 131] [122 132]] [[1 1] [246 257]] [[2 8] [252 263]] [[1 1] [282 292]] [[1 1] [329 339]] [[1 1] [360 370]] [[1 1] [380 390]] [[8 1] [382 392]] [[1 1] [384 395]] [[1 1] [385 396]] [[8 1] [390 401]] [[1 1] [391 403]] [[1 1] [392 404]] [[2 4] [393 405]] [[4 8] [395 407]] [[1 7] [471 482] [472 483]] [[1 1] [473 485]] [[2 1] [485 497]] [[1 7] [495 507] [496 508]] [[1 7] [518 531] [519 532]] [[1 7] [593 607] [594 608]] [[1 1] [599 614]] [[8 1] [600 615]] [[1 7] [672 688] [673 689]] [[8 4] [726 743]] [[8 4] [727 745]] [[1 1] [743 762]] [[1 7] [744 763] [745 764]] [[8 4] [746 766]] [[8 4] [747 768]] [[7 1] [1015 1037] [1016 1038]] [[1 1] [1023 1044]] [[8 1] [1024 1045]] [[4 4] [1158 1180]] [[1 1] [1169 1191]] [[8 1] [1170 1192]] [[2 8] [1302 1325]] [[5 8] [1324 1346]] [[1 7] [1385 1406] [1386 1407]] [[2 8] [1390 1412]] [[1 1] [1498 1519]] [[1 1] [1499 1520]] [[8 1] [1500 1521]] [[8 1] [1500 1522]] [[1 1] [1501 1524]] [[1 7] [1510 1533] [1511 1534]] [[1 7] [1618 1642] [1619 1643]] [[5 8] [1661 1686]] [[1 7] [1745 1769] [1746 1770]] [[1 1] [1821 1846]] [[1 1] [1849 1874]] [[8 4] [1885 1910]] [[1 1] [1926 1952]] [[4 4] [1933 1959]] [[8 4] [1968 1994]] [[8 5] [1968 1995]] [[8 4] [1968 1996]] [[8 4] [1968 1997]] [[1 1] [1981 2011]] [[1 7] [1982 2012] [1983 2013]] [[1 1] [1998 2029]] [[8 2] [2050 2081]] [[8 4] [2051 2083]] [[2 1] [2097 2130]] [[8 5] [2098 2131]] [[8 4] [2098 2132]] [[4 8] [2112 2147]] [[1 1] [2168 2202]] [[8 1] [2169 2203]] [[2 4] [2177 2212]] [[8 5] [2178 2213]] [[8 4] [2178 2214]] [[8 4] [2179 2216]] [[8 4] [2195 2233]] [[1 7] [2217 2256] [2218 2257]] [[1 7] [2272 2312] [2273 2313]] [[1 1] [2273 2313]] [[1 7] [2324 2365] [2325 2366]] [[1 1] [2333 2375]] [[2 8] [2431 2473]] [[7 1] [2473 2514] [2474 2515]] [[1 1] [2590 2630]] [[8 1] [2591 2631]] [[5 8] [2678 2719]] [[1 7] [2686 2726] [2687 2728]] [[1 7] [2688 2730] [2689 2731]] [[1 1] [2736 2779]] [[1 7] [2771 2814] [2772 2815]] [[1 1] [2810 2854]] [[1 7] [2811 2855] [2812 2856]] [[8 1] [2812 2857]] [[1 1] [2862 2908]] [[8 1] [2863 2909]] [[1 7] [2869 2916] [2870 2917]] [[1 1] [2982 3030]] [[1 7] [3020 3068] [3021 3069]] [[1 1] [3047 3096]] [[1 1] [3049 3098]] [[1 1] [3050 3099]] [[8 1] [3051 3100]] [[1 7] [3056 3106] [3057 3108]] [[1 7] [3060 3112] [3061 3113]] [[8 4] [3070 3123]] [[8 5] [3070 3124]] [[8 5] [3070 3125]] [[8 4] [3070 3126]] [[8 4] [3070 3127]]))))

(big-error-codes-test)
