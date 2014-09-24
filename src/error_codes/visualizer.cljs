(ns error-codes.visualizer
  (:require [domina :as d]
            [cljs.reader :as reader]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [clojure.string :as string]))

(defmulti get-position (fn [a b] a))

(defmethod get-position :substitution [_ [_ [l r]]]
  [[l (inc l)] [r (inc r)]])

(defmethod get-position :many-to-one [_ [_ [ls rs] [le re]]]
  [[ls (inc le)] [rs re]])

(defmethod get-position :one-to-many [_ [_ [ls rs] [le re]]]
  [[ls le] [rs (inc re)]])

(defmethod get-position :insertion [_ [_ [l r]]]
  [[l l] [r (inc r)]])

(defmethod get-position :deletion [_ [_ [l r]]]
  [[l (inc l)] [r r]])


(def empty-sign "|")

(defn get-text [[start end] text]
    (let [t (.substring text start end)]
    (if-not (= t "")
      t
      empty-sign)))

(defn make-highlight [error type a b]
  (let [positionlr (get-position type error)
        textlr (mapv get-text positionlr [a b])
        color (get {:substitution "#00FFFF" :insertion "green" :deletion "red"
                    :one-to-many "yellow" :many-to-one "orange"} type)]
    (mapv (fn [pos text lr]
            {:type type
             :position pos
             :color color
             :text text
             :error error
             :id (str error "-" lr)}) positionlr textlr ["l" "r"])))

(defn get-type [error]
  (let [[a b] (first error)]
    (cond
      (= a 8)  :insertion
      (= b 8)  :deletion
      (= b 7)  :one-to-many
      (= a 7)  :many-to-one
      :else    :substitution)))

(defn build-highlight [error texta textb]
  (let [type (get-type error)]
    (make-highlight error type texta textb)))


(defn build-highlights [errors a b]
  (as->
    (reduce (fn [[posl hls-left posr hls-right] error]
              (let [[{[ls le] :position :as hl-l} {[rs re] :position :as hl-r}]
                     (build-highlight error a b)]
                [le (conj hls-left (.substring a posl ls) hl-l)
                 re (conj hls-right (.substring b posr rs) hl-r)])) [0 [] 0 []] errors) x
     (let [[le hls re hlr] x]
       [(conj hls (.substring a le (count a))) (conj hlr (.substring b re (count b)))])))

(defn highlight-view [highlight owner]
  (reify
    om/IRender
    (render [this]
         (if (string? highlight)
           (do
           (dom/span nil (.replace highlight #"\r?\n" "\\n")))
           (dom/span #js {:style #js {:backgroundColor (:color highlight)}
                          :id (:id highlight)}
                     (.replace (:text highlight) #"\r?\n" "\\n"))))))


(defn mark [error-code]
  (let [idl (str error-code "-l")
        idr (str error-code "-r")]
    (d/add-class! (d/by-id idl) "boxed")
    (d/add-class! (d/by-id idr) "boxed")
    (js/setTimeout (fn []
                      (prn "unsetting")
                       (d/remove-class! (d/by-id idl) "boxed")
                       (d/remove-class! (d/by-id idr) "boxed")) 2000)))

(defn error-code-link-view [[error-code left right] owner]
  (reify
    om/IRender
    (render [this]
      (let [[hl hr] (build-highlight error-code left right)]
        (dom/a #js {:onClick (fn [& args] (mark error-code))}
               (str (first error-code) ":" (:text hl) "->" (:text hr)))))))



(defn table-view [[error-codes left right] owner]
  (reify
    om/IRender
    (render[this]
      (let [categories (group-by get-type error-codes)]
      (dom/div #js {:className "table-div"}
         (dom/table #js {:className "table"
                         :id (str "table")}
            (apply dom/tbody nil
              (dom/tr nil
                (dom/th nil "Fehlerart") (dom/th nil "Fehler") (dom/th nil "Anzahl"))
               (for [[k codes] categories]
                 (dom/tr nil
                   (dom/td nil (name k))
                   (apply dom/td nil (om/build-all error-code-link-view
                                 (map (fn [a] [a left right]) codes)))
                   (dom/td nil (str (count codes))))))))))))


(defn page-summary-view [{:keys [error-codes left right name]} owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "summary-div"}
               (dom/p nil name)
               (om/build table-view [error-codes left right])))))


(defn page-view [page owner]
  (reify
    om/IRender
    (render [this]
      (let [[hl hr] (build-highlights (:error-codes page) (:left page) (:right page))
            _ (prn "hl hr " hl hr)]
       (dom/div #js {:className "wrap"}
         (om/build page-summary-view page)
         (apply dom/div #js {:className "left"}
                   (om/build-all highlight-view hl ))
         (apply dom/div #js {:className "right"}
                    (om/build-all highlight-view hr)))))))
