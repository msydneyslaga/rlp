(ns main
  (:require [clojure.spec.alpha :as s]
            ["react-ace$default" :as AceEditor]
            ["ace-builds/src-noconflict/mode-haskell"]
            ["ace-builds/src-noconflict/theme-solarized_light"]
            ["ace-builds/src-noconflict/keybinding-vim"]
            [wscljs.client :as ws]
            [wscljs.format :as fmt]
            [cljs.core.match :refer-macros [match]]
            [hmvis.annotated :as annotated]
            [reagent.core :as r]
            [reagent.dom :as rdom]))

; (def *editor
;   (doto (js/ace.edit "editor")
;         (.setTheme "ace/theme/solarized_light")
;         (.setKeyboardHandler "ace/keyboard/vim")
;         (.setOption "mode" "ace/mode/haskell")))

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

(defonce *socket (ws/create "ws://127.0.0.1:9002"
                            {:on-message on-message
                             :on-open #(println "socket opened")
                             :on-close #(println "socket closed")
                             :on-error #(println "error: " %)}))

(defn send [msg]
  (ws/send *socket msg fmt/json))

(defonce *editor nil)

(defn TypeCheckButton []
  [:button {:id "type-check-button"
            :on-click #(send {:command "annotate"
                              :source (.getValue *editor)})}
   "type-check"])

(defn Editor []
  [:div {:class "editor-container"}
    [(r/adapt-react-class AceEditor)
     {:mode "haskell"
      :theme "solarized_light"
      :keyboardHandler "vim"
      :defaultValue (str "id = \\x -> x\n"
                         "flip f x y = f y x\n"
                         "fix f = letrec x = f x in x")
      :style {:width "100%"
              :height "100%"}
      :on-load (fn [editor]
                 (set! *editor editor)
                 (set! (.. editor -container -style -resize) "both")
                 (js/document.addEventListener
                   "mouseup"
                   #(.resize editor)))
      :name "editor"} ]])

(defn Main []
  [:<>
    [:div {:class "main-view-container"}
     [TypeCheckButton]
     [Editor]
     [annotated/TypeChecker]
     #_ [:div {:id "type-check-output"}
      "doge soge quoge"]]
   #_ [annotated/TypeChecker]])

;; start is called by init and after code reloading finishes
(defn ^:dev/after-load start []
  (rdom/render [Main]
               (js/document.getElementById "mount"))
  (js/console.log "start"))

(defn init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (js/console.log "init")
  (start))

;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (js/console.log "stop"))

