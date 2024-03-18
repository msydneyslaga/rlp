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

(defn Annotation [text visible?]
  (if visible?
      [:div {:class "annotation"}
       text]
      nil))

(def nesting-rainbow (cycle ["red" "orange" "yellow"
                             "green" "blue" "purple"]))

(defn Typed [t child]
  (let [hovering? (r/atom false)]
    (fn []
        [:div {:class "annotation-wrapper"
               :on-mouse-enter #(reset! hovering? true)
               :on-mouse-leave #(reset! hovering? false)}
         child
         [Annotation t @hovering?]])))

(declare Expr)

(defn LambdaExpr [binds body]
  [:<>
    [:code
      (hsep "λ" (apply hsep binds) "-> ")]
    [Expr 0 body]])

(defn VarExpr [var-id]
  [:code var-id])

(defn AppExpr [f x]
  [:<> [Expr ppr/app-prec f]
       " "
       [Expr ppr/app-prec1 x]])

(defn Expr [p {e :e t :type}]
  (match e
    {:InL {:tag "LamF" :contents [bs body & _]}}
      (maybe-parens (< ppr/app-prec1 p)
                    [Typed t [LambdaExpr bs body]])
    {:InL {:tag "VarF" :contents var-id}}
      [Typed t [VarExpr var-id]]
    {:InL {:tag "AppF" :contents [f x]}}
      (maybe-parens (< ppr/app-prec p)
                    [Typed t [AppExpr f x]])
    :else [:code "<expr>"]))

(defn render-decl [{name :name body :body}]
  [:code {:key name :display "block"}
           (str name " = ") [Expr 0 body] #_ (render-expr body)
           [:br]])

(defn type-checker []
  [:div
   (map render-decl (or @tc-input []))])

(defn init []
  (rdom/render [type-checker]
               (js/document.querySelector "#output")))

