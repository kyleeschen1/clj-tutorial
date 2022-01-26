(ns ^:figwheel-hooks clj-tutorial.core
  (:require

   [cljsjs.waypoints]
   [cljsjs.d3]
   
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

;;(al/run-eval-loop ev-stream config)



;;##########################################################################
;; Scroll Triggered Actions
;;##########################################################################

(defn set-scroll-trigger

  "Sets some action for when
   a particular element hits
   the viewport."

  [id f]
  
  (let [element (.getElementById js/document id)
        params {:element element
                :handler f}]
    
    (js/Waypoint. (clj->js params))))




;;##########################################################################
;; Main Page
;;##########################################################################

(defonce app-state
  (atom {:text "Jowls"}))

(def sidebar-width 300)
(def text-width 400)
(def repl-width 500)
(def repl-left-margin
  (+ 50 sidebar-width text-width))


(defn text->divs
  [text]
  (->> text
       (map (fn [[div text]]
              (let [id (str (gensym))]     
                [div {:id id} text])))
       (into [:span#text])))

(def text
  (text->divs
   
   [[:h1 "Introduction"]
    
    [:h1 "Syntax"]
    [:p "The history and mystery"]
    [:h2 "Arithmetic"]
    [:h2 "Lists"]
    [:h2 "Conditionals"]
    [:h2 "Bindings"]
    [:h2 "Functions"]
    [:p "I love lambdas..."]
    
    [:h1 "Recursion"]
    [:h2 "Basic Examples"]
    [:h3 "Sum"]
    [:h3 "Multiply"]
    [:h3 "Exponentiate"]
    [:h3 "Factorial"]
    [:h2 "Functions Over Lists"]
    [:h3 "Count"]
    [:h3 "Member?"]
    [:h3 "Replace"]
    [:h3 "Nth"]
    [:h3 "Reverse"]
    [:h3 "Append"]
    [:h2 "Higher Order Functions"]
    [:h3 "Map"]
    [:h3 "Walk"]
    [:h3 "Filter"]
    [:h3 "Reduce"]
    [:h2 "Searching and Sorting"]
    [:h3 "Bubble Sort"]
    [:h3 "Quicksort"]
    [:h2 "Conclusion"]
    
    [:h1 "The Meta-Circular Interpreter"]
    [:h2 "The Eval / Apply Loop"]
    [:h2 "S-Expressions"]
    [:h2 "Symbols & Environment"]
    [:h2 "Conditionals"]
    [:h2 "Functions"]
    [:p "Why would we even do this?"]
    
    [:h1 "The Y-Combinator"]]))




;;===========================================
;; Headers -> Hyperlinks
;;===========================================

(def h->level 
  {:h1 1
   :h2 2
   :h3 3
   :h4 4})

(def sidebar-link-color
  "#818181")

(defn header?
  [div]
  (and (vector? div)
       (h->level (first div))))

(defn header->link
  [h]
  (let [header-id (:id (second h))
        header-text (last h)]
    [:li 
     [:a
      {:href (str "#" header-id)
       :id (str "a-" header-id)
       :style {:text-decoration "none"
               :color sidebar-link-color
               :&:hover {:background-color "light-blue"}}}

      header-text]]))

(defn headers->links

  "Converts a flat hiccup vector of 
   headers interspersed with other div
   types, and creates a nested list of
   links that reflect the tree structure."
  
  [headers]
    
  (loop [[h & hs] headers
         level 0
         acc []
         parent-stack []]
    
    (cond
      
      (nil? h)
      (reduce (fn [acc parent]
                (conj parent acc))
              acc
              (reverse parent-stack))

      
      (header? h)
      (let [current-level (h->level (first h))

            list-element (header->link h)
            
            [acc* ps*] (cond
                         
                         (> current-level level)
                         
                         (let [ul (with-meta
                                    [:ul list-element]
                                    {:level current-level})]
                           
                           (vector ul
                                   
                                   (if (empty? acc)
                                     parent-stack
                                     (conj parent-stack acc))))
                         
                         
                         (< current-level level)
                         ;; There's a bug here if we jump from a :h3 to
                         ;; a :h1... we need to do multiple pops / peeks
                         (vector (-> parent-stack
                                     (peek)
                                     (conj acc)
                                     (conj list-element))
                                 (pop parent-stack))
                         
                         :else
                         (vector (conj acc list-element) 
                                 parent-stack))]
        
        (recur hs current-level acc* ps*))

      :else
      (recur hs level acc parent-stack))))




(defn select
  [string]
  (js/d3.select string))

(defn select-by-id
  [id]
  (js/d3.select (str "#" id)))

(defn classed
  [obj class active?]
  (.classed obj class active?))

(defn style
  [obj name value]
  (.style obj name value))



(defn hiccup?
  [div]
  (vector? div))

(defn populate-outline-with-headers!
  [text]
  (doseq [t text]
    
    (when (hiccup? t)
      
      (let [id (:id (second t))
            
            element (select-by-id (str "a-" id))

            change-color (fn [obj turn-on? color]
                           (-> obj
                               (classed "highlighted" turn-on?)
                               (style "color" color)))

            f (fn [_]
                
                (when-let [prior (select ".highlighted")]
                  (change-color prior false sidebar-link-color))

                (change-color element true "white" ))]
        
        (set-scroll-trigger id f)))))

(defn sidebar [text]
  [:div {:id "sidebar"
         :style {:height "100vh"
                 :width sidebar-width
                 :z-index 1
                 :background-color "#111"
                 :padding-top "20px"
                 :padding-left "20px"
                 :overflow-x "hidden"
                 :position "fixed"
                 :top "0px"
                 :left "0px"
                 :font-weight "400"
                }}
   [:div {:style {:font-size "12px"
                  :line-height "1.6"
                  :color "#818181"
                  :display "block"
                  :background-color "transparent"
                  :text-decoration "none"}}
    
    [:h1 "Animated Clojure"]
    (headers->links text)]])


(defn text-column
  
  [text]
  
  (let [style {:margin-left sidebar-width
               :margin-right "20px"
               :width text-width
               :padding "20px 30px 30px 40px"
               :float "left"
               :font-size "18px"
               :overflow "scroll"}]
    
    [:span {:style style} text]))



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
                  }}
  (code-g) ])




(defn main []
    
  [:div {:style {:display "flex"
                 :font-family "Verdana"
                 :font-size "15px"}}

   (text-column text)
   (sidebar text)
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
    (mount el)
    (populate-outline-with-headers! text)))

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
