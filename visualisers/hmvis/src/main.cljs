(ns main
  (:require [clojure.spec.alpha :as s]
            [wscljs.client :as ws]
            [wscljs.format :as fmt]
            [cljs.core.match :refer-macros [match]]
            [hmvis.annotated :as annotated]
            [reagent.dom :as rdom]))

(def *editor
  (doto (js/ace.edit "editor")
        (.setTheme "ace/theme/solarized_light")
        (.setKeyboardHandler "ace/keyboard/vim")
        (.setOption "mode" "ace/mode/haskell")))

(def *output (.querySelector js/document "#output"))

(defn display-errors [es]
  (doseq [{{e :contents} :diagnostic} es]
         (let [fmte (map #(str "  • " % "\n") e)]
           (js/console.warn (apply str "message from rlpc:\n" fmte)))))

(defn with-success [f ma]
  (match ma
    {:errors es :result nil} (display-errors es)
    {:errors es :result a}   (do (display-errors es)
                                 (f a))))

(defn on-message [e]
  (let [r (js->clj (js/JSON.parse (.-data e)) :keywordize-keys true)]
    (match r
           {:tag "Annotated" :contents c}
             (with-success #(reset! annotated/tc-input %) c)
           :else
             (js/console.warn "unrecognisable response from rlp"))))

(def *socket (ws/create "ws://127.0.0.1:9002"
                        {:on-message on-message
                         :on-open #(println "socket opened")
                         :on-close #(println "socket closed")
                         :on-error #(println "error: " %)}))

(defn send [msg]
  (ws/send *socket msg fmt/json))

(defn init-type-check-button []
  (let [b (.querySelector js/document "#type-check")]
    (.addEventListener b "click"
                       #(send {:command "annotate"
                               :source (.getValue *editor)}))))

;; start is called by init and after code reloading finishes
(defn ^:dev/after-load start []
  ; (rdom/render [type-checker] (js/document.getElementById "output"))
  (annotated/init)
  (js/console.log "start"))

(defn init []
  (init-type-check-button)
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (js/console.log "init")
  (start))

;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (ws/close *socket)
  (js/console.log "stop"))

