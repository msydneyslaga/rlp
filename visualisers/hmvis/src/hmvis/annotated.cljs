(ns hmvis.annotated
  (:require [cljs.core.match :refer-macros [match]]
            [cljsx.core :refer [jsx> react> defcomponent]]
            [react :as react]
            [react-dom :as react-dom]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.pprint :refer [cl-format]]
            [hmvis.ppr :as ppr]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]))

(defonce tc-input (r/atom nil))

(defonce current-annotation-text (r/atom nil))

(defn unicodify [s]
  (str/replace s #"->" "→"))

(defn punctuate [p & as]
  (match as
         [] ""
         _  (reduce #(str %1 p %2) as)))

(defn hsep [& as]
  (apply punctuate " " as))

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
         [Annotation colour (unicodify t) hovering?]])))

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

(defn let-or-letrec [rec]
  (match rec
         "Rec"    "letrec"
         "NonRec" "let"))

(defn Pat [colours p {:keys [tag contents]}]
  (match tag
         "VarP" contents))

(defn Binding [colours {:keys [tag contents]}]
  (match tag
         "VarB" (let [[p v] contents]
                  [:<> [Pat colours 0 p] " = " [Expr colours 0 v]])))

(defn LetExpr [colours rec bs e]
  [:<> (let-or-letrec rec)
       " "
       (apply punctuate "; " (map (partial Binding colours) bs))
       " in "
       (Expr colours 0 e)])

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
    {:InR {:tag "LetEF" :contents [r bs body]}}
      (maybe-parens (< ppr/app-prec1 p)
                    [Typed c t [LetExpr colours r bs body]])
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

