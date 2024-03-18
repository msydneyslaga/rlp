(ns hmvis.annotated
  (:require [cljs.core.match :refer-macros [match]]
            [cljsx.core :refer [jsx> react> defcomponent]]
            [react :as react]
            [react-dom :as react-dom]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.pprint :refer [cl-format]]
            [hmvis.ppr :as ppr]))

(defonce tc-input (r/atom nil))

(defonce current-annotation-text (r/atom nil))

(defn hsep [& as]
  (let [f (fn [a b] (str a " " b))]
    (reduce f as)))

(defn maybe-parens [c s]
  (if c
      [:<> "(" s ")"]
      s))

(defn formatln [fs & rest]
  (apply cl-format true (str fs "~%") rest))

(def nesting-rainbow (cycle ["red" "orange" "yellow"
                             "green" "blue" "purple"]))

(defn text-colour-by-background [colour]
  (match colour
         "yellow" "black"
         _        "white"))

(defn Annotation [colour text hovering?]
  [:div {:class (if @hovering?
                    "annotation hovering"
                    "annotation")
         :on-mouse-enter #(reset! hovering? true)
         :on-mouse-leave #(reset! hovering? false)
         :style {:background colour
                 :color (text-colour-by-background colour)}}
   [:div {:class "annotation-text"}
    text]])

(defn Typed [colour t child]
  (let [hovering? (r/atom false)]
    (fn []
        [:div {:class "annotation-wrapper"}
         [:div {:class (if @hovering?
                           "typed-wrapper hovering"
                           "typed-wrapper")
                }
          [:div {:class "code-wrapper"} child]]
         [Annotation colour t hovering?]])))

(declare Expr)

(defn LambdaExpr [colours binds body]
  [:<>
    [:code
      (hsep "λ" (apply hsep binds) "-> ")]
    [Expr colours 0 body]])

(defn VarExpr [var-id]
  [:code var-id])

(defn AppExpr [colours f x]
  [:<> [Expr colours ppr/app-prec f]
       " "
       [Expr colours ppr/app-prec1 x]])

(defn Expr [[c & colours] p {e :e t :type}]
  (match e
    {:InL {:tag "LamF" :contents [bs body & _]}}
      (maybe-parens (< ppr/app-prec1 p)
                    [Typed c t [LambdaExpr colours bs body]])
    {:InL {:tag "VarF" :contents var-id}}
      [Typed c t [VarExpr var-id]]
    {:InL {:tag "AppF" :contents [f x]}}
      (maybe-parens (< ppr/app-prec p)
                    [Typed c t [AppExpr colours f x]])
    :else [:code "<expr>"]))

(def rainbow-cycle (cycle ["red"
                           "orange"
                           "yellow"
                           "green"
                           "blue"
                           "violet"]))

(defn render-decl [{name :name body :body}]
  [:code {:key name :display "block"}
           (str name " = ") [Expr rainbow-cycle 0 body] #_ (render-expr body)
           [:br]])

(defn TypeChecker []
  [:div
   (map render-decl (or @tc-input []))])

; (defn init []
;   (rdom/render [type-checker]
;                (js/document.querySelector "#output")))

