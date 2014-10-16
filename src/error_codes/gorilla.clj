;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla visualisations for error codes.
;;; 
;;; Inline views for error codes are a huge help when correcting pages and viewing the changes made by correction algorithms.
;; **

;; @@
(ns error-codes.gorilla
  (:require [error-codes.core :as e]
            [gorilla-renderable.core :as render]
            [marmoset.core :as m]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(m/defsegment diff-view
              {:preamble ["react.js"]
               :optimizations :advanced
               :pretty-print false
               :externs ["react.js"]}
  (ns error-codes.visualized
    (:require [marmoset.client :as m]
              [om.core :as om :include-macros true]
              [error-codes.visualizer :as v]))
  (enable-console-print!)
  (defn main []
    (marmoset.client/autoheight)
    (om/root v/page-view
             m/env
             {:target (.item (.getElementsByTagName js/document "body") 0)}))
  (set! (.-onload js/window) main))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;error-codes.gorilla/diff-view</span>","value":"#'error-codes.gorilla/diff-view"}
;; <=

;; @@
(defrecord ErrorCodeView [left right error-codes]
  render/Renderable
  (render [self]
          (let [id (java.util.UUID/randomUUID)
                env  {:left left
                      :right right
                      :error-codes error-codes}
                html (diff-view id env)]
            {:type :html
             :value (pr-str self)
             :content html})))

(defn error-view [left right]
  (->ErrorCodeView left
                   right
                   (e/error-codes left right)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;error-codes.gorilla/error-view</span>","value":"#'error-codes.gorilla/error-view"}
;; <=
