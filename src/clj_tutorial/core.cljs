(ns ^:figwheel-hooks clj-tutorial.core
  (:require

   [clj-tutorial.html.d3 :as d3]
   [clj-tutorial.html.scrolling :refer [set-scroll-trigger]]
   [clj-tutorial.html.headers-to-hyperlinks :refer [headers->links
                                                    populate-outline-with-headers!]]

  
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
(comment
  (def code-text
    "(if (+ 1 2)
    (* 3 4)
    (- 3 4))")

  (def code*
    (string->code code-text))

  (def ev-stream
    (t/gen-evaluation-stream code*))

  (defn code-g []
    [:g (code->hiccup code*)]))

(def config
  (atom {:pause-time 2000}))

;;(al/run-eval-loop ev-stream config)





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

;;#######################################################
;; Text Compiler
;;#######################################################

(def text*
   
  [[:h1 "Introduction"]

   
   
   [:h1 "Syntax"]
   [:p "The history and mystery"]
   [:h2 "Arithmetic"]
   [:h2 "Lists"]
   [:h2 "Conditionals"]
   [:h2 "Bindings"]
   [:h2 "Functions"]
   [:p "I love lambdas..."]

   '(add-code 'factorial

              (defn factorial [n]
                (if (zero? n)
                  1
                  (* n (factorial n)))))
   
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
   
   [:h1 "The Y-Combinator"]])

;;(def ^:dynamic data)

;;#######################################################
;; Adding Ids, Values, Display Info
;;#######################################################

(defrecord Const [val])

(defn const?
  [form]
  (instance? Const val))

(defn token?
  [form]
  (or (const? form)
      (not (coll? form))))

(defn postwalk*
  [f form]
  
  (let [f* (fn [form*]
             (if-not (satisfies? IMeta form*)
               form*
               (with-meta form* (meta form))))]

    (clojure.walk/walk (partial postwalk* f*) f* form)))

(defn annotate-code-with-ids
  [code]
  (letfn [(annotate [code]
            
            (let [display (when-not (coll? code)
                            (str code))
                  
                  code (if (satisfies? IMeta code)
                          code
                          (Const. code))]
              
              (with-meta code {:id (gensym "token_")
                               :form code
                               :display display})))]

    (clojure.walk/postwalk annotate code)))



;;#######################################################
;; Adding Newlines & Indents
;;#######################################################


(def special-forms
  
  "With some macros thrown in!"
  
  '#{if cond do fn defn let loop})

(defmulti add-newlines-&-indents
  
  (fn [form]
    
    (cond
      
      (vector? form) :vector
      
      (list? form) (let [[f & _] form]
                     (get special-forms f :s-exprs)))))



(defmethod add-newlines-&-indents :default
  [form]
  form)

(defmethod add-newlines-&-indents :s-exprs
  [form]
  form)

(defmethod add-newlines-&-indents 'if
  [[op pred then else]]
  
  (let [v [op pred ::newline ::indent then]
        
        v (if else
            
            (conj v [::newline else])
            
            v)]

    (list* v)))

(defmethod add-newlines-&-indents 'cond
  [[op & forms]]
  (let [insert-newline-between (fn [[pred then]]
                                 [pred ::newline then])
        spaced-pairs (->> forms
                          (partition 2)
                          (map insert-newline-between)
                          (interleave (repeat [::newline ::newline]) )
                          (apply concat))]
    
    (list* op
           ::indent
           spaced-pairs)))

(defmethod add-newlines-&-indents 'do
  [[op & statements]]
  (list* op
         ::indent
         (interleave (repeat ::newline)
                     statements)))

(defmethod add-newlines-&-indents 'fn
  [[op params & body]]
  (list* op
         params
         ::newline
         ::indent
         (interleave body (repeat ::newline))))

(defmethod add-newlines-&-indents 'defn
  [[op fname sym params & body]]
  (list* op
         params
         ::newline
         ::indent
         (interleave body (repeat ::newline))))

(defn add-newlines-&-indents-to-binding-form
  [[op bindings & body]]
  
  (let [bindings* (loop [[sym val & rst :as b] bindings
                         acc (with-meta [] (meta bindings))]
                    
                    (if-not b
                      
                      (pop acc) ;; eliminates closing newline
                      
                      (recur rst (conj acc sym val ::newline))))]
    
    (list* op
           bindings*
           ::newline
           ::indent
           (interleave body (repeat ::newline)))))

(defmethod add-newlines-&-indents 'let
  [form]
  (add-newlines-&-indents-to-binding-form form))

(defmethod add-newlines-&-indents 'loop
  [form]
  (add-newlines-&-indents-to-binding-form form))





;;#######################################################
;; Building D3 Data from Code
;;#######################################################

(declare coll->d3-data
         tag-token
         tag-coll
         add-bracket-data)

(defn form->d3-data
  
  ([form]
   
   (form->d3-data form 0 0 []))
  
  ([form x y data]
   
   (if (token? form)
     
     (tag-token form x y)

     (coll->d3-data form x y data))))
     
(defn coll->d3-data
  
  [form x y data]
  
  (let [form-w-newlines-&-indents (add-newlines-&-indents form)
        
        [x-original y-original] [x y]

        ;; Increment to adjust for the opening bracket
        x (inc x)]

    (loop [[e & es :as elements] form-w-newlines-&-indents
           x x
           y y
           indent x
           data []]
      
      (cond

        (not (seq elements))
        (let [{:keys [x-end y-end]} (last data)

              ;; create bracket data
              brackets (add-bracket-data form
                                           x-original
                                           y-original
                                           x-end
                                           y-end)

             
              
              ;; Increment to adjust for outer bracket
              x-end (inc x-end) ]
          
          (conj data
               ;; (conj brackets)
                (tag-coll form
                          x-original
                          y-original
                          x-end
                          y-end)))
        
        (= e ::newline)
        (recur es
               indent ;; return x to indent position
               (inc y)
               indent
               data)

        (= e ::indent)
        (recur es
               (inc x)
               y
               (inc indent)
               data)

        :else
        (let [element-data (form->d3-data e x y data)

              [final-frame data] (if (vector? element-data)
                                   
                                   (vector (last element-data)
                                           (into data element-data))
                                   
                                   (vector element-data
                                           (conj data element-data)))

              {:keys [x-end y-end]} final-frame

              ;; Increment to adjust for space between tokens
              x-end (inc x-end)]

          (recur es
                 x-end
                 y-end
                 indent
                 data))))))

(defn length
  [form]
  (count (str form)))

(defn tag-token
  [form x y]
  (let [val (if (const? form)
              (:val form)
              form)
        l (length val)
        m (meta form)]

    (assoc m
           :x x
           :y y
           :x-end (+ x l)
           :y-end y)))

(defn tag-coll
  [form x y x-end y-end]
  
  (let [m (meta form)]
    
    (assoc m
           :x x
           :y y
           :x-end x-end
           :y-end y-end)))

(def type->brackets
  {cljs.core/List ["(" ")"]
   cljs.core/PersistentArrayMap ["{" "}"]
   cljs.core/PersistentVector ["[" "]"]
   cljs.core/PersistentHashSet ["#{" "}"]})

(defn add-bracket-data
  [form x y x-end y-end]
  
  (let [[op cl] (type->brackets (type form))

        id (-> form meta :id)
        
        gen-id (fn [prefix]
                 (-> prefix
                     (str id)
                     (keyword)))]

    (vector {:display op
             :id (gen-id "opening-")
             :bracket :opening
             :x x
             :y y}
            
            {:display cl
             :id (gen-id "closing-")
             :bracket :closing
             :x x-end
             :y y-end})))

(def toast
  (annotate-code-with-ids '(if (zero? 1)
                             a
                             b)))




;;#######################################################
;; Adding Code to Dom
;;#######################################################


(defn d3-data->dom!
  [data]
  (-> (d3/select "#repl")
      (.selectAll ".code")
      (.data data)
      (.enter)
      (.append "text")
      (.text (fn [d]
               (.-display d)))
      (.attr "x" (fn [d]
                    (.-x d)))
      (.attr "y" (fn [d]
                    (.-y d)))))

;;#######################################################
;; Compiling Text
;;#######################################################


(def code
  (atom nil))


(defmulti compile
  (fn [[tag & _] state]
    tag))

(defmethod compile :default
  [[tag text] state]
  
  (let [id (str (gensym))
        
        hiccup [tag {:id id} text]]

    (-> state
        (assoc :current-id id)
        (update :hiccup conj hiccup))))

(defmethod compile 'add-code
  [[_ & [code-id & forms]] {:keys [current-id] :as state}]

  (let [;;hiccup (code->hiccup (cons 'do forms))
        d3-data (-> (cons 'do forms)
                    (annotate-code-with-ids)
                    (form->d3-data))
        action {:type :add-to-dom
                :trigger-id current-id
                :action (fn []
                          (pprint d3-data)
                          (d3-data->dom! d3-data)
                          ;;(reset! code hiccup)
                          )}]

    (update state :actions conj action)))


(defn postprocess
  
  [{:keys [hiccup] :as state}]
  
  (let [outline (headers->links hiccup)]

    (-> state
        (assoc :outline outline))))


(defn compile-text
  [text]
  
  (loop [[t & ts] text
         state {:hiccup []
                :actions []
                :current-id nil
                :current-animation-state nil}]

    (if-not t

      (postprocess state)

      (recur ts (compile t state)))))

(defn add-scroll-triggers!
  
  [state]
  
  (doseq [actions (:actions state)]

    (let [{:keys [trigger-id action]} actions]

      (set-scroll-trigger trigger-id action))))

;;#######################################################
;; Main Page Columns
;;#######################################################




(defn sidebar [state]
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
    (:outline state)]])


(defn text-column
  
  [text]
  
  (let [style {:margin-left sidebar-width
               :margin-right "20px"
               :width text-width
               :padding "20px 30px 30px 40px"
               :float "left"
               :font-size "18px"
               :overflow "scroll"}]
    
    (into [:span#text {:style style}] (:hiccup text))))



;; https://www.w3schools.com/howto/howto_css_fixed_sidebar.asp
(defn repl-column [] 
  [:div  {:style {:margin-left repl-left-margin
                
                  :padding "0px 0px"
                  :z-index 1
                  ;;:width repl-width
                  :height "300px"
                  :flex "50%"

                  :float "left"
                  :white-space "pre-wrap"
                  :border "10px solid black"
           
                  :font-size "18px"
                  :overflow "scroll"
                  :position "fixed"

                  }
          :id "repl-col"}

   [:svg#repl]])

(def t
  (compile-text text*))


(defn main []
    
  [:div {:style {:display "flex"
                 :font-family "Verdana"
                 :font-size "15px"}}

   (text-column t)
   (sidebar t)
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
    (populate-outline-with-headers! (:hiccup t))
    (add-scroll-triggers! t)))

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
