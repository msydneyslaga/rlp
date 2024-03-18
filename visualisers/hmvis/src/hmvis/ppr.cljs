(ns hmvis.ppr
    (:require [cljs.core.match :refer-macros [match]]))

(def app-prec  10)
(def app-prec1 11)

(defn- maybe-parens [c s]
  (if c
      (str "(" s ")")
      s))

(defn- hsep [& as]
  (let [f (fn [a b] (str a " " b))]
    (reduce f as)))

(declare expr)

(defn lambda-expr [binds body]
  (hsep "λ" (apply hsep binds) "->" (expr body)))

(defn app-expr [f x]
  (hsep (expr app-prec f) (expr app-prec1 x)))

(defn var-expr [var-id]
  var-id)

(defn expr
  ([exp] (expr 0 exp))

  ([p {e :e}]
    (match e
           {:InL {:tag "LamF" :contents [bs body & _]}}
           (maybe-parens (< app-prec1 p)
                         (lambda-expr bs body))
           {:InL {:tag "VarF" :contents var-id}}
           (var-expr var-id)
           {:InL {:tag "AppF" :contents [f x]}}
           (maybe-parens (< app-prec p)
                         (app-expr f x))
           :else [:code "<expr>"])))

