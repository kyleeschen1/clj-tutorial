(ns ^:figwheel-hooks clj-tutorial.core
  (:require

   [clj-tutorial.interpreter.tracer :as t]
   [clj-tutorial.code-to-hiccup :refer [code->hiccup]]
   [clj-tutorial.parser :refer [string->code]]

   [clj-tutorial.animation.animation-loop :as al]
   
   [clojure.repl :refer [source]]
   [clojure.pprint :refer [pprint]] 

   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]))



;;##########################################################################
;; Code Processing
;;##########################################################################

(def code-text
"(if (+ 1 2)
     (* 3 4)
     (- 3 4))")

(def code
  (string->code code-text))

(def ev-stream
  (t/gen-evaluation-stream code))

(defn code-g []
  [:g (code->hiccup code)])

(def config
  (atom {:pause-time 2000}))

(al/run-eval-loop ev-stream config)

;;##########################################################################
;; Styles
;;##########################################################################


;;##########################################################################
;; Main Page
;;##########################################################################

(defonce app-state
  (atom {:text "Jowls"}))

(def sidebar-width 200)
(def text-width 400)
(def repl-width 500)
(def repl-left-margin
  (+ 50 sidebar-width text-width))

(def text
  [:p "Toast wlfnrj;wmkgrthytjukjhgrferghytjukjht grertyetrukjyhtgrrhjuytreqwtyeut\n
Toast wlfnrj;wmkgrthytjukjhgrferghytjukjhtgrertyetrukjyhtgrrhjuytreqwtyeu
Toast wlfnrj;wmkgrthytjukjhgrferghytjukjhtgrertyetrukjyhtgrrhjuytreqwtyeut\n
Toast wlfnrj;wmkgrthytjukjhgrferghytjukjht grertyetrukjyhtgrrhjuytreqwtyeut\n
Toast wlfnrj;wmkgrthytjukjhgrferghytjukjhtgrertyetrukjyhtgrrhjuytreqwtyeu
Toast wlfnrj;wmkgrthytjukjhgrferghytjukjhtgrertyetrukjyhtgrrhjuytreqwtyeut\n"])

(defn sidebar []
  [:div {:style {:height "100vh"
                 :width sidebar-width
                 :z-index 1
                 :background-color "#111"
                 :padding-top "20px"
                 :overflow-x "hidden"
                 :position "fixed"
                 :top "0px"
                 :left "0px"}}
   [:a {:style {:padding "6px 8px 6px 16px"
                :font-size "25px"
                :color "#818181"
                :display "block"}
        :hover {:color "#f1f1f1"}} "Lemon"]])


(defn text-column []
  [:span {:style {:margin-left sidebar-width
                 :width text-width
                 :padding "20px 20px 20px"
                 :float "left"
                 :font-size "18px"
                 :overflow "scroll"
                 :white-space "normal"
                  :overflow-wrap "break-word"
                 }}
   text])



;; https://www.w3schools.com/howto/howto_css_fixed_sidebar.asp
(defn repl-column [] 
  [:svg  {:style {:margin-left repl-left-margin
                
                  :padding "0px 0px"
                  :z-index 1
                  :width repl-width
                  :height "300px"
                  :flex "50%"

                  :white-space "pre-wrap"
                  :border "10px solid black"
           
                  :font-size "18px"
                  :overflow "scroll"
                  :position "fixed"
                 ;; :background-color "red"
                  }}
   (code-g)
   ])


(defn button []
  [:input {:type "button"
            :value "Click"
            :on-click (fn [_]
                     ;;   (pprint @config)
                        (swap! config update :pause-time * 0.66))} ])

(defn main []
  [:div {:style {:display "flex"}}
   (sidebar)
   (button)
   (text-column)
   (repl-column) ])




;;##########################################################################
;; Mounting Elements
;;##########################################################################

(defn get-app-element []
  (gdom/getElement "app"))


(defn mount [el]
  (rdom/render [main] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

;; conditionally start your application based on the presence of an "app" element
;; this is particularly helpful for testing this ns without launching the app
(mount-app-element)

;; specify reload hook with ^:after-load metadata
(defn ^:after-load on-reload []
  (mount-app-element)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
