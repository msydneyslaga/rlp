(ns hmvis.annotated
  (:require [cljs.core.match :refer-macros [match]]
            ; [cljsx.core :refer [jsx> react> defcomponent]]
            ; [react :as react]
            ; [react-dom :as react-dom]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [clojure.pprint :refer [cl-format]]))

(defonce tc-input (r/atom nil))

(defonce current-annotation-text (r/atom nil))

(def app-prec  10)
(def app-prec1 11)

(defn hsep [& as]
  (let [f (fn [a b] (str a " " b))]
    (reduce f as)))

; (defn maybe-parens [c s]
;   (if c
;       (react> (<> "(" s ")"))
;       s))

(defn formatln [fs & rest]
  (apply cl-format true (str fs "~%") rest))

(defn Typed [t & children]
  (formatln "type: ~S" t)
   [:div {:class "annotation-wrapper"
          :onMouseEnter #(do (println "doge")
                             (reset! current-annotation-text "doge"))
          :onMouseLeave #(reset! current-annotation-text nil)}
      children])

(defn Annotation []
  [:p (or @current-annotation-text "<nil>")])

(declare Expr)

(defn LambdaExpr [binds body]
  [:<>
    [:code
      (hsep "λ" (apply hsep binds) "-> ")]
    [Expr 0 body]])

(defn VarExpr [var-id]
  [:code var-id])

(defn AppExpr [f x]
  [:<> [Expr app-prec f]
       " "
       [Expr app-prec1 x]])

(defn Expr [p {e :e t :type}]
  (match e
    {:InL {:tag "LamF" :contents [bs body & _]}}
      [LambdaExpr bs body]
    {:InL {:tag "VarF" :contents var-id}}
      [VarExpr var-id]
    {:InL {:tag "AppF" :contents [f x]}}
      [AppExpr f x]
    :else [:code "<expr>"]))

(defn render-decl [{name :name body :body}]
  [:code {:key name :display "block"}
           (str name " = ") [Expr 0 body] #_ (render-expr body)
           [:br]])

(defn Thing []
  [:h1 @current-annotation-text])

(defn type-checker []
  [:div
   [Thing]
   #_ [:button {:on-click #(reset! current-annotation-text "doge")}]])

; (defcomponent TypeChecker props
;   (react>
;     (<div>
;       (<Thing>)
;       (<button :onClick #(do (reset! current-annotation-text "doge")
;                              (formatln "thing: ~S" @current-annotation-text)) >)
;       #_ (map render-decl (or @tc-input [])))))

(defn init []
  (rdom/render [type-checker]
               (js/document.querySelector "#type-check")))

