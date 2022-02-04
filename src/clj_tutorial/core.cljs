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

   [cljs.core.async :refer [chan close! >! <! put!]]

   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom])
   (:require-macros
    [cljs.core.async.macros :as m :refer [go go-loop ]]))



(comment
  ;;#######################################################
  ;; Treemap
  ;;#######################################################

  (def stratify
    (-> (js/d3.stratify)
        (.id (fn [d] (:id d)))
        (.parentId (fn [d] (:parent-id d)))))

  (def tree
    (-> '(if (zero? 0) (+ 1 2) (* 3 4))
        annotate-code-with-ids
        form->d3-data
        into-array
        stratify))

  (defn node-get

    ([d kw]
     (get (.-data d) kw))

    ([kw]
     (fn [d]
       (get (.-data d) kw))))

  (defn tree-add-default-properties
    
    [obj x-anchor y-anchor]
    
    (modify obj
            
            :font-size (get-cf-param :font-size)
            
            :id (fn [d]
                  (str "text-" (:id (.-data d))))
            
            :x (fn [d]
                 (+ x-anchor
                    (* (get-cf-param :font-size)
                       (get-cf-param :x-scale)
                       (:x (.-data d)))))
            
            :y (fn [d]
                 (+ y-anchor
                    (* (get-cf-param :font-size)
                       (get-cf-param :y-scale)
                       (:y (.-data d)))))
            
            :fill (fn [d]
                    (let [{:keys [form bracket]} (.-data d)]
                      (cond
                        bracket "grey"
                        (special? form) "purple"
                        (number? form) "grey"
                        (function? form) "blue"
                        :else "black")))
            
            :opacity (fn [d]
                       (let [{:keys [bracket]} (.-data d)]
                         (if bracket
                           0.4
                           1)))))


  (defn add-clicker
    []
    (-> (js/d3.selectAll ".code")
        (.on "click" (fn [node]
                       
                       (let [d (.-data node)]

                         (pprint d)
                         
                         (when (:bracket d)

                           (pprint d)

                           (let [p (.-parent node)]
                             
                             (-> p
                                 (.each (fn [node]
                                          
                                          (let [id (:id (.-data node))]
                                            
                                            (-> (js/d3.select (str "#text-" id))
                                                (.style "fill" "red")
                                                (.transition)
                                                (.duration 1000)
                                                (.style "fill" "black")))))))))))))

  (defn build-tree []
    (-> (get-repl)
        (.selectAll "g")
        (.data (.descendants tree))
        (.enter)
        (.append "text")
        (.text (node-get :display))
        (.classed "code" true)
        (.transition)
        (.duration 1000)
        (tree-add-default-properties 0 100))

    (add-clicker))

  (comment
    (-> (js/d3.selectAll ".code")
        (.on "click" (fn [node]
                       
                       (let [d (.-data node)]

                         (pprint d)
                         
                         (when (:bracket d)

                           (pprint d)

                           (let [p (.-parent node)]
                             
                             (-> p
                                 (.each (fn [node]
                                          
                                          (let [id (:id (.-data node))]
                                            
                                            (-> (js/d3.select (str "#text-" id))
                                                (.style "fill" "red")
                                                (.transition)
                                                (.duration 1000)
                                                (.style "fill" "black"))))))))))))))

(comment
  (defn form->dom!
    [form]
    
    (let [id (:id (meta form))
          
          id-attr {:id (str id)
                   :class "code"}]
      
      (if (token? form)

        [:g id-attr]
        
        (let [elements (if (map? form)
                         
                         (apply concat form)
                         
                         (seq form))

              op [:g {:id (str "opening-" id)
                      :class "code"}]
              
              cl [:g {:id (str "closing-" id)
                      :class "code"}]
              
              divs (mapv form->dom! elements)]
          
          (into [:g id-attr op cl] divs)))))

  (defn add-to-dom
    [hiccup]
    
    (let [el (gdom/getElement "repl")]
      
      (rdom/render hiccup el))))

(comment
  (defmulti get-ff-&-rr
    (fn [key _]
      key))

  (defn compose
    [& fns]
    (apply comp (reverse fns)))
  
  (defmethod get-ff-&-rr :modify
    [_ pairs]
    (let [animation-id (gensym "animation_")
          former-vals (atom {})

          record! (fn [id key]
                    
                    (when id
                      
                      (let [current (-> (str "#" id)
                                        (js/d3.selectAll)
                                        (aspect key))]
                        
                        (swap! former-vals
                               update-in
                               [id]
                               (fnil conj {})
                               {key current}))))

          ff (fn [key f]
               
               (fn [{:keys [id] :as d}]

                 (record! id key)

                 (if (or (number? f) (string? f))
                   
                   f
                   
                   (f d))))

          rr (fn [key _]
               
               (fn [{:keys [id]}]
                 
                 (get-in @former-vals [id key])))


          gen-property-fn (fn [transform-f [key f]]

                            (let [f* (transform-f key f)]

                              (fn [obj]

                                (aspect obj key f*))))

          chain-property-fns (fn [transform-f pairs]
                               (->> pairs
                                    (map (partial gen-property-fn transform-f))
                                    (apply comp)))
          
          forward (chain-property-fns ff pairs)
          
          rewind (chain-property-fns rr pairs)

          initiated? (atom false)
          
          forward (fn [obj]
                    (let [obj* (forward obj)]
                      (reset! initiated? true)
                      obj))

          rewind (fn [obj]
                   (if @initiated?
                     (rewind obj)
                     obj))]
      
      [forward rewind]))

  (defmethod get-ff-&-rr :append
    [_ data]
    (let [animation-id (gensym "animation-")
          cache (atom nil)
          ff (fn [obj]
               (->  obj
                    (.selectAll ".code")
                    (.data data)
                    (.enter)
                    (.append "text")
                    (.classed animation-id true)
                    (.transition)
                    (.duration 500)
                    (.text (by-key :display))
                    (add-default-properties 0 0)))
          rr (fn [obj]

               (let [obj* (.selectAll obj (str "#" animation-id)) ]

                 (.remove obj*)))]
      [ff rr]))

  (defmethod get-ff-&-rr :remove
    [_ data]
    (let [[ff rr] (get-ff-&-rr :append data)]
      
      [rr ff]))



  (defn gen-transition-fn
    
    [config]

    (let [{:keys [ease duration delay]
           :or {ease js/d3.easeLinear
                duration 0
                delay 0}} config]
      
      (fn [obj]
        
        (-> obj
            (.transition (str (gensym)))
            ;;(.ease ease)
            (.duration duration)
            (.delay delay)))))

  (defn compile-animation
    
    [{:keys [select transition append remove modify]}]

    (let [s (if select
              select
              (fn [_]
                (-> (get-repl config)
                    (.selectAll "text"))))
          
          t (if transition
              (gen-transition-fn transition)
              identity)

          transforms
          (for [key [:modify]
                config [modify]]

            (when config
              (->> (get-ff-&-rr key config)
                   (mapv (partial compose s t)))))

          ff (map first transforms)
          rr (map second transforms)]

      {:ff ff
       :rr rr}))

  (defn compile-animations [& configs]
    
    (let [animations (map compile-animation configs)
          ffs (map :ff animations)
          rrs (map :rr animations)]
      
      {:ff (flatten ffs)
       :rr (reverse (flatten rrs))}))


  (def x
    (compile-animations
     
     {:transition {:duration 1500}
      :modify {:x 200 :y 300 :fill "blue"}}

     {:transition {:delay 250 :duration 1500}
      :modify {:x 300 :y 200 :fill "green"}}
     
     {:transition {:duration 1500}
      :modify {:x 300 :y 100 :fill "red"}}))

  








(defn fns->thunks [fns & args]
  (map (fn [f]
         (fn []
           (apply f args)))
       fns))

(defn add-click! []
  (-> (get-repl config)
      (.on  "click" (fn [d]               
                      (let [obj (-> (get-repl config)
                                    (.selectAll "text"))
                            {:keys [ff rr] } x
                            thunks (fns->thunks (concat ff rr)
                                                obj)]

                        (add-events! true thunks))))))
)

;;#######################################################
;; Adding Ids, Values, Display Info
;;#######################################################

(defrecord Const [val])

(defn const?
  [form]
  (instance? Const form))

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

(defn add-ids
  [code]
  (letfn [(annotate [code]
            
            (let [display (when-not (coll? code)
                            (str code))
                  
                  code* (if (satisfies? IMeta code)
                          code
                          (Const. code))]
              
              (with-meta code* {:id (gensym "token_")
                                :form code
                                :display display})))]

    (clojure.walk/postwalk annotate code)))



;;#######################################################
;; Adding Newlines & Indents
;;#######################################################

(defmulti add-x-y
  
  (fn [form]
    
    (cond
      
      (vector? form) :vector
      
      (list? form) (let [special? '#{if cond do fn defn}
                         binding? '#{loop let}
                         [f & _] form]
                     
                     (cond

                       (special? f) f
                       (binding? f) 'binding-forms
                       :else :s-exprs)))))



(defmethod add-x-y :default
  [form]
  form)

(defrecord Pos [dx dy element])

(defmethod add-x-y :s-exprs
  [[f & args :as form]]
  
  (if (some (complement token?) args)
    
    (let [dx (count (str f))
          args->indents (fn [arg]
                          (Pos. dx 1 arg))]
      (list* f
             (first args)
             (map args->indents (rest args))))
    
    form))

(defmethod add-x-y 'if
  [[op pred then else]]
  
  (let [v [op pred (Pos. 1 1 then)]
        
        v (if else
            
            (conj v (Pos. 0 1 else))
            
            v)]

    (list* v)))

(defmethod add-x-y 'cond
  [[op & forms]]
  (let [insert-newline-between (fn [[pred then]]
                                 [pred (Pos. 0 1 then)])
        spaced-pairs (->> forms
                          (partition 2)
                          (map insert-newline-between)
                          (interleave (Pos. 0 2 nil) )
                          (apply concat))]
    
    (list* op
           (Pos. 1 0 nil)
           spaced-pairs)))

(defmethod add-x-y 'do
  [[op & statements]]
  (list* op
         (Pos. 1 0 nil)
         (map #(Pos. 0 1 %) statements)))

(defmethod add-x-y 'fn
  [[op params & body]]
  (list* op
         params
         (Pos. 1 0 nil)
         (map #(Pos. 0 1 %) body )))

(defmethod add-x-y 'defn
  [[op fname params & body]]
  (list* op
         fname
         params
         (Pos. 1 0 nil)
         (map #(Pos. 0 1 %) body )))

(defmethod add-x-y 'binding-forms
  [[op bindings & body]]
  
  (let [bindings* (loop [[sym val & rst :as b] bindings
                         n 0
                         acc (with-meta [] (meta bindings))]
                    
                    (if-not b
                      
                      acc

                      (let [nl (if (zero? n)
                                   0
                                   1)]
                        (recur rst
                               (inc n)
                               (conj acc (Pos. 0 nl sym) val)))))]
    
    (list op
          bindings*
          (Pos. 1 1 body))))







;;#######################################################
;; Building D3 Data from Code
;;#######################################################

(declare coll->data
         gen-token-data
         gen-coll-data
         gen-bracket-data)

(def spaces
  {:indent 1.5
   :after-opening 0.5
   :before-closing 0.5
   :between-tokens 2})

 
(defn code->data

  ([form]
   (code->data form 0 0 []))

  ([form x y]
   (code->data form x y []))
  
  ([form x y data]

   (if (token? form)

     (gen-token-data form x y)
     
     (let [form* (add-x-y form)
           
           [x0 y0] [x y]

           ;; Adjust for the opening bracket
           x (+ x 1 (:after-opening spaces))]

       (loop [[e & es :as elements] form*
              x x
              y y
              indent x
              data []]
         
         (cond


           (and  (not (seq elements))
                 (not (:id (meta form))))
           data

           (not (seq elements))
           (let [{:keys [x-end y-end]
                  :or {x-end (+ (:between-tokens spaces) x0)
                       y-end y0}} (last data)

                 ;; create bracket data
                 [op cl] (gen-bracket-data form
                                           x0
                                           y0
                                           x-end
                                           y-end)

                 ;; Properly sequence brackets and data
                 data (conj (into [op] data) cl)
                 
                 ;; Adjust for closing bracket
                 x-end (+ x-end
                          (:between-tokens spaces)
                          (:before-closing spaces))

                 ;; create coll data
                 coll-data (gen-coll-data form
                                          x0
                                          y0
                                          x-end
                                          y-end)

                 ;; Add child ids
                 coll-data (->> data
                                (map :id)
                                (set)
                                (assoc coll-data :child-ids))]

             (into [coll-data] data))

           (instance? Pos e)
           (let [{:keys [dx dy element]} e
                 
                 y (+ y dy)
                 
                 indent (+ indent (* dx (:indent spaces)))

                 data (if-not element
                        
                        (conj data {:position true
                                    :dy dy
                                    :indent indent})

                        (let [data* (code->data element indent y indent)

                              path [0 :dy]

                              data* (assoc-in data* path dy)]
                          
                          (into data data*)))

                 x (+ (:between-tokens spaces) indent)]
             
             (recur es
                    x
                    y
                    indent
                    data))


           :regular-element
           (let [frames (code->data e x y data)

                 [last-fr data] [(last frames) (into data frames)]

                 {:keys [x-end y-end]} last-fr

                 ;; Increment to adjust for space between tokens
                 x-end (+ (:between-tokens spaces) x-end)]

             (recur es
                    x-end
                    y-end
                    indent
                    data))))))))


(defn length
  [form]
  (count (str form)))

(defn gen-token-data
  [form x y]
  (let [val (if (const? form)
              (:val form)
              form)
        l (length val)
        m (meta form)]

    (vector
     
     (assoc m
            :x x
            :y y
            :x-end (+ x l)
            :y-end y))))

(defn gen-coll-data
  [form x y x-end y-end]
  
  (let [m (meta form)]
    
    (assoc m
           :x x
           :y y
           :x-end x-end
           :y-end y-end)))

(defn coll->brackets [form]
  (cond
    (map? form) ["{" "}"]
    (vector? form) ["[" "]"]
    (set? form) ["#{" "}"]
    true ["(" ")"]))

(defn gen-bracket-data
  [form x y x-end y-end]
  
  (let [[op cl] (coll->brackets form)

        id (-> form meta :id)
        
        gen-id (fn [prefix]
                 (str prefix id))

        cl-x (+ x-end (:before-closing spaces))]

    (vector {:display op
             :id (gen-id "opening-")
             :bracket :opening
             :x x
             :y y}
            
            {:display cl
             :id (gen-id "closing-")
             :bracket :closing
             :x cl-x
             :y y-end
             :x-end (inc cl-x)
             :y-end y-end})))

;;#######################################################
;; Adding Code to Dom
;;#######################################################

(defn by-key
  [id]
  (fn [data]
    (get data id)))

(def config
  {:svg-id "repl"
   :svg-height 800
   :svg-width 500
   
   :font-size 12
   :x-scale 0.5
   :y-scale 3})

(defn attr
  
  ([obj key]
   (.attr obj (name key)))
  
  ([obj key f]
   (.attr obj (name key) f)))

(defn style
  
  ([obj key]
   (.style obj (name key)))
  
  ([obj key f]
   (.style obj (name key) f)))

(def attr?
    #{:x :y :height :width :id :class })

(defn aspect
  [obj key & args]
  
  (let [f (if (attr? key)
            attr
            style)]

    (apply f (list* obj key args))))




(def special?
  '#{if cond let loop def fn defn do quote})

(def function?
  '#{cons first rest empty? list?})



(defn modify
  [obj & args]
  (letfn [(gen-aspect-fn [[key f]]
            (fn [obj]
              (aspect obj key f)))]

    (let [f (->> args
               (partition 2)
               (map gen-aspect-fn)
               (apply comp))]
      
      (f obj))))



(defn get-cf-param
  [param]
  (get config param))

(defn get-repl

  ([]
   (get-repl config))
  
  ([cf]
   (d3/select-by-id (cf :svg-id))))

(defn clear-repl!
  []
  (-> (get-repl)
      (.selectAll "text")
      (.transition)
      (.duration 1000)
      (.style "opacity" 0)
      (.remove)))

(defn add-default-properties
  
  [obj x-anchor y-anchor]
  
  (modify obj
          
          :font-size (get-cf-param :font-size)
          
          :id (fn [d]
                (str (:id d)))
          
          :x (fn [d]
               (+ x-anchor
                  (* (get-cf-param :font-size)
                    (get-cf-param :x-scale)
                    (:x d))))
          
          :y (fn [d]
               (+ y-anchor
                  (* (get-cf-param :font-size)
                     (get-cf-param :y-scale)
                     (:y d))))
          
          :fill (fn [{:keys [form bracket]}]
                  (cond
                    bracket "grey"
                    (special? form) "purple"
                    (number? form) "grey"
                    (function? form) "blue"
                    :else "black"))
          
          :opacity (fn [{:keys [bracket]}]       
                     (if bracket
                       0.4
                       1))))


(defn select-descendants-by-id
  [id]
  (let [p (js/d3.select (str "#" id))
        child-ids (:child-ids  (.datum p))
        descendant? (fn [d]
                       (child-ids (:id d)))]

    (-> (js/d3.selectAll ".code")
        (.filter descendant?))))

(defn add-clicker*
  []
  (-> (js/d3.selectAll ".code")
      (.on "hover" (fn [d]
                     (js/console.log d)))
      (.on "click" (fn [d]

                     (let [selection (if (:bracket d)
                                       (select-descendants-by-id (:parent-id d))
                                       (js/d3.selectAll (str "#" (:id d))))]
                       
                       (-> selection
                           (.style "fill" "red")
                           (.transition)
                           (.duration 2000)
                           (.style "fill" "black")))))))


(defn add-data-to-repl

  ([data]
   (add-data-to-repl data {}))
  
  ([data {:keys [selection-fn
                 id-prefix
                 g-id
                 append-g?
                 delay-time
                 duration-time
                 x-start
                 y-start
                 x-anchor
                 y-anchor]
          :or {selection-fn get-repl
               id-prefix ""
               append-g? true
               delay-time 0
               duration-time 1000
               x-start 0
               y-start 0
               x-anchor 0
               y-anchor 100}}]
     
   (-> (selection-fn)
       (.selectAll (str (gensym)))
       (.data (into-array data))
       (.enter)
       
       ((fn [obj]
          (if-not append-g?
            obj
            (-> obj
                (.append "g")
                (.attr "id" (fn [d]
                              (str id-prefix (:id d))))
                (.classed "code" true)))))
       
       (.append "text")
       (.classed "code" true)
       (.style "font-size" 0)
       (.attr "x" x-start)
       (.attr "y" y-start)
       
       (.transition)
       (.delay delay-time)
       (.duration duration-time)
       
       (.text (by-key :display))
       (.style "font-size" (get-cf-param :font-size))
          
       (.attr "id" (fn [d]
                   (str "text-" id-prefix (:id d))))
          
       (.attr "x" (fn [d]
                  (+ x-anchor
                     (* (get-cf-param :font-size)
                        (get-cf-param :x-scale)
                        (:x d)))))
          
       (.attr "y"  (fn [d]
                      (+ y-anchor
                         (* (get-cf-param :font-size)
                            (get-cf-param :y-scale)
                            (:y d)))))
          
       (.style "fill"  (fn [{:keys [form bracket]}]
                        (cond
                          bracket "grey"
                          (special? form) "purple"
                          (number? form) "grey"
                          (boolean? form) "grey"
                          (function? form) "blue"
                          :else "black")))
          
       (.style  "opacity" (fn [{:keys [bracket]}]       
                          (if bracket
                            0.4
                            1))))))



;;######################################################
;; Async
;;######################################################

(def animation-channel
  (chan))


(defn block
  [obj]
  (let [c (chan)]
    (.on obj "end" (fn [] (close! c)))
    c))

(go-loop []

  (when-let [[block? thunk] (<! animation-channel)]

    (let [thunk* (thunk)]

      (when block?
        (<! (block thunk*)))))
  
  (recur))

(defn add-event!
  [block? thunk]
  (put! animation-channel [block? thunk]))

(defn add-events!
  [block? [t & ts]]
  (when t
    (add-event! block? t)
    (recur block? ts)))

;;#######################################################
;; Compiling Text
;;#######################################################


(defn add-action

  ([state action-fn]
   (add-action state action-fn false))

  ([state action-fn pre?]
   
   (let [id (:current-id state)
         id (if pre?
              (str "pre-" id)
              id)
         action {:trigger-id id
                 :action action-fn}]

     (-> state
         (update :actions conj action)))))

(defmulti -compile-page
  (fn [state [tag & _]]
    tag))

(def header?
  #{:h1 :h2 :h3 :h4})

(defmethod -compile-page :default
  [state [tag text]]
  
  (let [id (str (gensym))
        
        hiccup [tag {:id id} text]]

    (-> state
        (assoc :current-id id)
        (update :hiccup conj  [:div {:id (str "pre-" id)}
                               
                               [:br {:id (str (gensym))}]
                               
                             ;;  [:br {:id (str (gensym))}]
                               ])
        (update :hiccup conj hiccup)
        (add-action (fn [direction]
                      
                      (when-let [obj (d3/select-by-id id)]

                        (when-let [former (d3/select ".in-focus")]
                          (-> former
                              (.classed "in-focus" false)
                              (.style "color" "black")))
                        
                        (-> obj
                            (.classed "in-focus" true)
                            (.style "color" "red")) ))))))


(def current-code
  (atom {:current-data-id nil
         :code {}}))

(declare eval-in)

(def evaluate-button
  
  [:div

   {:id (gensym "eval_button_")}

   [:input {:type "button"
            :value "Evaluate"
            :on-click #(eval-in (let [id (:current-data-id @current-code)
                                      forms (get-in @current-code [:code id :forms])]
                                  (if (token? forms)
                                    forms
                                    (cons 'do forms))))}]])
(defn add-evaluate-button
  [state]
  (update state :hiccup conj evaluate-button))

(defn gen-forms-to-dom-fn
  [code-id forms]

  (fn []
    ;; clear existing text
    (-> (js/d3.selectAll ".code")
        (.remove))

    ;; update current code id
    (swap! current-code assoc :current-data-id code-id)

    ;; If code is already processed,
    ;; add to repl.
    ;;
    ;; Otherwise, process forms, then
    ;; add data to both the state and repl.
    ;; 
    (if-let [params (get-in @current-code [:code code-id])]
 
      (add-data-to-repl (:data params))
      
      (let [forms (map add-ids forms)

            forms (cons (first forms)
                        (map #(Pos. 0 1 %) (rest forms)))
            data (code->data forms  0 0)

            mp {:forms forms :data data}]

        (swap! current-code assoc-in [:code code-id] mp)
        
        (add-data-to-repl data)))))


(defmethod -compile-page 'add-code
  
  [state [_ & [code-id & forms]]]

  (let [restore-previous-code-fn (:restore-previous-code-fn state)

        forms-to-dom-fn (gen-forms-to-dom-fn code-id forms)
        
        action-fn (fn [direction]
                    (if (= "down" direction)
                      (forms-to-dom-fn)
                      (restore-previous-code-fn)))]

    
    (-> state
        (assoc :restore-previous-code-fn forms-to-dom-fn)
        (add-action action-fn true)
        (add-evaluate-button))))


(defmethod -compile-page 'add-animation
  
  [state [_ & animation]]

  (let [{:keys [ff rr]} state ;;(apply compile-animation  animation)

        action-fn  (fn [direction]
                     (let [i (-> (get-repl config)
                                 (.selectAll "text"))]
                       (if (= "down" direction)
                         (ff i)
                         (rr i))) )]

    (add-action state action-fn)))



(defn postprocess
  
  [{:keys [hiccup] :as state}]
  
  (let [outline (headers->links hiccup)]

    (-> state
        (assoc :outline outline))))

(defn compile-page
  [& text]
  
  (let [state {:hiccup []
               :actions []
               :height 4
               :restore-previous-code-fn (fn []
                                           (-> (js/d3.selectAll ".code text")
                                               (.remove)))
               :current-id nil
               :current-animation-state nil}]

    (->> text
         (reduce -compile-page state)
         (postprocess))))


(defn add-scroll-triggers!
  
  [state]

  (populate-outline-with-headers! (:hiccup state))
  
  (doseq [actions (:actions state)]

    (let [{:keys [trigger-id action]} actions]

      (set-scroll-trigger trigger-id action))))



;;##########################################################################
;; Evaluator
;;##########################################################################



(defmulti gen-animation-fn :type)

(defn apply-to-brackets
  [f id]
  (f (js/d3.select (str "#text-opening-" id)))
  (f (js/d3.select (str "#text-closing-" id))))

(defn undo []
  (-> (get-repl)
      (.selectAll ".under-evaluation")
      (.classed "under-evaluation" false)
      (.transition)
      (.duration 1000)
      (.style "fill" "black")))

(defmethod gen-animation-fn :default
  
  [{:keys [form result env id] :as frame}]

  (let [highlight (fn highlight [obj]
                    (-> obj
                        (.selectAll "text")
                        (.classed "under-evaluation" true)
                        (.transition)
                        (.duration 500)
                        (.style "fill" "red")))]

    (add-event! true (fn []
                       
                       (if (token? form)

                         (highlight (d3/select-by-id (str id)))

                         (apply-to-brackets highlight id))))))

(defn g-sel->text-sel
  [selection]
  (js/d3.select (str "#text-" (:id (.datum selection)))))

(defn selection->attrs
  [selection & attrs]
  
  (let [s (g-sel->text-sel selection)]
    
    (mapv (fn [k]
            (-> (.attr s (name k))
                (js/parseInt)))
          
          attrs)))

(defn fade-replaced-text!
  [selection]
  (let [[x y] (selection->attrs selection :x :y) ]
    
    (add-event! true (fn []
                 
                      (-> selection
                          (.selectAll "text")
                          (.transition)
                          (.duration 1000)
                          (.style "fill" "red")
                          (.transition)
                          (.duration 500)
                          (.attr "x" x)
                          (.attr "y" y)
                          (.style "font-size" 0)
                          (.style "opacity" 0))))))

(defn replace-selection-with-result
  [selection result id]
    
  (when (not (fn? result))

    (fade-replaced-text! selection)
   
    ;; Add result in place
    (let [[x y] (selection->attrs selection :x :y)  
          data (code->data result)]

      (add-event! true (fn []
                         (add-data-to-repl data {:selection-fn (fn []
                                                                 (d3/select-by-id id))
                                                 :append-g? false
                                                 :duration-time 200
                                                 :x-start x
                                                 :y-start y
                                                 :x-anchor x
                                                 :y-anchor y}))))))


(defmethod gen-animation-fn :s-exprs
  [{:keys [form result env id] :as frame}]
  (-> (select-descendants-by-id id)
      (replace-selection-with-result result id)))

(defmethod gen-animation-fn :tail-call
  [{:keys [form result env id] :as frame}]
  (-> (select-descendants-by-id id)
      (replace-selection-with-result result id)))


(defmethod gen-animation-fn :symbol
  [{:keys [form result env id] :as frame}]

  (-> (d3/select-by-id id)
      (replace-selection-with-result result id)))


(defmethod gen-animation-fn :tail-call-
  [{:keys [form result env id] :as frame}]
  
  (let [result-id (-> result meta :id)
        selection (-> (select-descendants-by-id id)
                      (.filter (fn [d]
                                  (not (= result-id (:id d))))))]

    (fade-replaced-text! selection)
    (comment
      (add-event! true (fn []
                         (-> selection
                             (.selectAll "text")
                             (.filter (fn [d]
                                        (not (= result-id (:id d)))))
                             (.transition)
                             (.duration 500)
                             (.style "fill" "red")
                             (.style "opacity" 0)))))
 

    (let [[x y] (selection->attrs selection :x :y)]

      (add-event! true (fn []
                         (-> (js/d3.select (str "#text-" (:id (meta result)) ))
                             (.transition)
                             (.duration 500)
                             (.attr "x" x)
                             (.attr "y" y)))))))



(defn animate-frame!
  [frame]
  (add-event! false undo)
  (gen-animation-fn frame))

(comment

    Resolution types
    - Add to locals
    - Add to globals
    - Symbol resolution
    - Recur
    - Lambda Expansion
    - S-expression resolution)

(defn form->id
  [form]
  (:id (meta form)))

(def skip?
  #{:constant :default})

(defn trace
  
  [key form result env]

  (when-not (skip? key)
    
    (when-let [id (form->id form)]
      
      (let [tail-call? #{:binding-form :if :do}
            implemented? #{:symbol :s-exprs}

            type (or
                  (when (tail-call? key)
                    :tail-call)
                  (implemented? key)
                  :default)

            result- (if-not (:id (meta result))
                      (add-ids result)
                      result)

            frame {:form form
                   :result result
                   :env env
                   :type type
                   :id (form->id form)}]
        
        (animate-frame! frame)))))

;;##########################################################
;; Symbols & Environment
;;##########################################################

(defn get-val
  [form]
  (if (instance? Const form)
    (:val form)
    form))

(defn number-fn->constant-fn

  [op]
  
  (fn const-op
    
    ([] 0)
    ([x] x)
    ([x y] (op (get-val x) (get-val y)))
    ([x y & more]
     (reduce const-op (const-op x y) more))))

(def const+
  (number-fn->constant-fn #'+))

(def const-
  (number-fn->constant-fn #'-))

(def const*
  (number-fn->constant-fn #'*))

(def const-div
  (number-fn->constant-fn #'/))

(defn fn->const-fn
  [op]
  (fn [const]
    (op (get-val const))))



(extend-protocol IEquiv

  Const
  (-equiv [o other]
    (if (instance? Const other)
      (= (:val o) (:val other))
      (= (:val o) other))))

(def default-globals
  {'zero? (fn->const-fn zero?)
   'inc (fn->const-fn inc)
   'dec (fn->const-fn dec)
   'first #'first
   'rest #'rest
   'list? #'list?
   'empty? #'empty?
   'cons #'cons
   '+ #'const+
   '- #'const-
   '* #'const*
   '/ #'const-div
   '= #'=})

(def ^:dynamic *global-env*)

(defn add-to-local-env
  [env sym val]
  (trace :add-to-env sym val env)
  (assoc-in env [:locals sym] val))

(defn add-to-global-env!
  [sym val]
  (trace :add-to-globals sym val *global-env*)
  (swap! *global-env* assoc sym val))

(defn resolve-symbol
  [form env]
  (or (get-in env [:locals form])
      (get @*global-env* form)
      (throw (str "Symbol " form " not found."))))


;;##########################################################
;; Evaluation
;;##########################################################



(def log-eval?
  (atom false))

(defn log-eval
  [form result]
  (when @log-eval?
        (println "=====================")
        (pprint form)
        (println "----->")
        (pprint result)))


(defn process-result
  [form result]
  
  (let [result (add-ids result)]

    result))

(defn tag-form
  [form]
  
  (let [special-form? '#{if cond do def defn fn  recur quote}
        binding-form? '#{let loop}
        macro? '#{cond when and or}]
    
    (cond
      (const? form) :constant
      (symbol? form) :symbol
      (map? form) :map
      (vector? form) :vector
      (set? form) :set
      (list? form) (let [[f & _] form]
                     (cond
                       (special-form? f) (keyword f)
                       (binding-form? f) :binding-form
                       (macro? f) :macro
                       :else :s-exprs)))))

(declare -evaluate)

(defn evaluate
  [form env]
  (try

    (let [result (-evaluate form env)
          
          result (if-not (form->id  result)
                   (process-result form result)
                   result)]

      (trace (tag-form form) form result env)

      result)
      
      
    (catch js/Error. e
      (pprint e)
      (pprint form)
      (pprint env))))

(defn eval-in
  [form]
  
  (binding [*global-env* (atom default-globals)]
    
    (evaluate form {})))

(defn evaluate-list
  [forms env]
  (doall
   (map (fn [form]
          (evaluate form env))
        forms)))


;;##########################################################
;; Evaluation Multimethod
;;##########################################################

(defmulti -evaluate 
  (fn [form _]    
    (tag-form form)))

(defmethod -evaluate :default
  [form env]
  form)

(defmethod -evaluate :constant
  [form env]
  form)

(defmethod -evaluate :symbol
  [form env]
  (resolve-symbol form env))

(defmethod -evaluate :quote
  [[_ form] env]
  form)

(defrecord Lambda [params body lexical-env])

(defn lambda?
  [form]
  (instance? Lambda form))

(defn evaluate-lambda
  [{:keys [params body lexical-env] :as lambda} args env]
 
  (let [bindings (zipmap params args)
        env (-> env
                (update :locals merge (:locals lexical-env))
                (update :locals merge bindings))]

   (trace :lambda-expansion (cons lambda args) body env)
    
   (evaluate body env)))


(defmethod -evaluate :s-exprs
  [[f & args :as form] env]
  
  (let [f (evaluate f env)
        args (evaluate-list args env)]

    (if (lambda? f)
      
      (evaluate-lambda f args env)
      
      (apply f args))))

(defmethod -evaluate :fn
  [[_ params body] env]
  
  (Lambda. params body env))

(defmethod -evaluate :defn
  
  [[_ sym params body] env]
  
  (let [f (Lambda. params body env)]
    
    (add-to-global-env! sym f)))

(defmethod -evaluate :def
  [[_ sym val] env]
  
  (let [val (evaluate val env)]

    (add-to-global-env! sym val)

    val))


(defmethod -evaluate :if
  [[_ pred then else] env]
  (let [branch (if (get-val (evaluate pred env))
                 then
                 else)]
    (evaluate branch env)))

(defmethod -evaluate :do
  [[_ & forms] env]
  
  (let [statements (butlast forms)
        return (last forms)]

    (evaluate-list statements env)
    
    (evaluate return env)))


(defmethod -evaluate :binding-form

  [[_ binding-vector & body] env]

  (let [pairs (partition 2 binding-vector)
        
        env (loop [[[sym val] & rst :as pairs*] pairs
                   env env]
              
              (if-not (seq pairs*)
                
                env
                
                (let [val (evaluate val env)]
                  
                  (recur rst
                         (add-to-local-env env sym val)))))
        
        body (cons 'do body)]
    
    (evaluate body env)))





;;#######################################################
;; Main Page Columns
;;#######################################################

(def sidebar-width 300)
(def text-width 400)
(def repl-width 500)
(def repl-left-margin
  (+ 50 sidebar-width text-width))

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

(defn code->color
  [code]
  (cond 
    (special? code) "purple"
    (number? code) "grey"
    (boolean? code) "grey"
    (function? code) "blue"
    :else "black"))

(defn code->styles
  [code]
  {:id (-> code meta :id)
   :fill (code->color code)
   :class "code-tspan"
   :opacity 1})

  
(defn code->tspans
  
  [code indent]
  
  (let [style (code->styles code)

        code (add-ids code)

        space [:tspan.space "  "]
        tspan [:tspan style space]

        letter-width 4
        space-width 4
        indent-width (* 4 space-width)
        line-height 36

        add-enclosings (fn [acc op cl]
                         (let [styles {:opacity 0.3}]
                           [[[:tspan#opening styles space op]]
                            acc
                            [[:tspan#closing styles space cl]]]) )

        map-tspan (fn [code op cl]

                    (loop [[c & cs :as code-seq] code
                           indent (+ space-width letter-width space-width indent)
                           x 0 
                           acc []] 
                      
                      (cond

                        (not (seq code-seq))

                        (->> (add-enclosings acc op cl)
                             (apply concat)
                             (into tspan))

                        (= ::indent c)
                        (recur cs (+ indent-width indent) x acc)

                        (= ::newline c)
                        (let [ts (code->tspans c indent)]
                          
                          (recur cs indent 0 (conj acc ts)))

                        :else
                        (let [indent* (if (token? c)
                                        indent
                                        (+ indent x))
                              
                              ts (code->tspans c indent*)
                              
                              x  (cond

                                   (const? c)
                                   (+ x (+ space-width (* letter-width (count (str (:val c))))))
                                   
                                   (symbol? c)
                                   (+ x (+ space-width (* letter-width (count (str c)))))
                                   
                                   :else x)]

                          (recur cs           
                                 indent
                                 x
                                 (conj acc ts))))))]
    

    (cond

      (= ::newline code)
      [:tspan.newline (assoc style
                             :dy line-height
                             :x indent
                             :font-size 0)
       " "]

      (= ::indent code)
      nil
      

      (const? code)
      (conj tspan (str (:val code)))

      (symbol? code)
      (conj tspan (str code))

      (list? code)
      (map-tspan code "(" ")")

      (vector? code)
      (map-tspan code "[" "]")

      (map? code)
      (map-tspan (apply concat code) "{" "}")

      (coll? code)
      (map-tspan code "(" ")")
      
      :else
      (conj tspan code))))


(def code*
  (conj [:text {:x 0 :y 100}]

                   (-> '(let [x 1
                              y 2]
                          (if (+ 1 2 3)
                            (fn [x]
                              (+ x 1))
                            (let [x 5
                                  y (let [z 3
                                          a b]
                                      (+ 1 2 a b c))
                                  r 3
                                  t 4]
                              (do 1 2 3))))
                       (add-ids)
                     
                       (code->tspans 0))))

(defn text-column
  
  [text]
  
  (let [style {:margin-left sidebar-width
               :margin-right "20px"
               :width text-width
               :padding "20px 30px 30px 40px"
               :float "left"
               :font-size "12px"
               :overflow "scroll"}

        svg [:svg#temp
           
           {:height 800
            :width text-width}

            code*

             (when false (conj
                          

                          [:text {:x 100 :y 300}]

                          [:tspan  "(" 

                           [:tspan "lmeon"]
                           [:tspan "Eggs"]
                           [:tspan {:dy 18
                                    :x 0} "Toast"]
                           [:tspan "Yo!"]]))]]
    
    ;;(conj [:div])

          
          
    (into [:span#text {:style style} svg] (:hiccup text))))



;; https://www.w3schools.com/howto/howto_css_fixed_sidebar.asp
(defn repl-column [] 
  [:div  {:style {:margin-left repl-left-margin
                
                  :padding "0px 0px"
                  :z-index 1
                ;;  :width repl-width
               ;;   :height "800px"
                  :flex "50%"

                  :float "left"
                  :white-space "pre-wrap"
                 ;; :border "10px solid black"
           
                  :font-size "18px"
                  :overflow "scroll"
                  :position "fixed"

                  }
          :id "repl-col"}

   [:svg#repl {:height 800 :width 500}]])




(defn main [page]
    
  [:div {:style {:display "flex"
                 :font-family "Verdana"
                 :font-size "15px"}}

   (text-column page)
   (sidebar page)
   (repl-column) ])


;;##########################################################################
;; Web Page
;;##########################################################################

'(add-animation

     :transition {:duration 1000}
     :modify {:x 200 :y 300 :fill "red"}
     
     :transition {:delay 100 :duration 1000}
     :modify {:x 300 :y 200 :fill "green"}
     
     :transition {:delay 100 :duration 500}
     :modify {:x 100 :y 100 :fill "blue"})

(def page*

  (compile-page
   
   [:h1 "Introduction"]

   
   [:h1 "Syntax"]
   [:p "The history and mystery"]
   [:h2 "Arithmetic"]

   '(add-code arithmetic

              (+ 1 3 (* 8 4)))
   
   [:h2 "Lists"]
   [:h2 "Conditionals"]
   [:h2 "Bindings"]

   '(add-code let

              (let [x (+ 4 5)
                    y 2]
                (if (zero? 1)
                  (+ x y)
                  (* x y))))

   [:h2 "Functions"]
   [:p "Lambda this, lambda that."]
   
   
   [:p "Something, something, lambdas..."]
   
   [:h1 "Recursion"]
   
   [:h2 "Functions Over Intergers"]
   [:h3 "Sum"]
   
   '(add-code sum
              (defn sum [x n]
                (if (zero? n)
                  x
                  (sum (inc x) (dec n)))))
   [:h3 "Multiply"]

   '(add-code multiply

              (defn multiply [x n]
                (if (zero? n)
                  x
                  (multiply (+ x x) (dec n)))))
   [:h3 "Exponentiate"]

   
   '(add-code exponentiate

              (defn power [x n]
                (if (zero? n)
                  x
                  (power (* x x) (dec n)))))
   
   [:h3 "Factorial"]

   '(add-code factorial

              (do
                (defn factorial [n]
                  (if (zero? n)
                    1
                    (* n (factorial (- n 1)))))

                (factorial 3)))
   
   [:h2 "Functions Over Lists"]
   [:h3 "Count"]
   [:h3 "Member?"]
   [:h3 "Replace"]
   [:h3 "Nth"]
   [:h3 "Reverse"]
   [:h3 "Append"]
   [:h2 "Higher Order Functions"]
   [:h3 "Map"]

   '(add-code map

              (do
                (defn map [f ls]
                  (if (empty? ls)
                    '()
                    (cons (f (first ls))
                          (map f (rest ls)))))

                (map inc '(1 2 3))))
   
   [:h3 "Walk"]
   [:h3 "Filter"]
   '(add-code filter

              (defn filter [pred ls]
                
                (if (empty? ls)
                  
                  '()
                  
                  (let [fst (first ls)]
                    
                    (if (pred fst)
                      
                      (cons fst (filter (rest ls)))
                      
                      (filter pred (rest ls))))))

              (filter odd? '(1 2 3)))
   [:h3 "Reduce"]

   '(add-code reduce

              (defn reduce [f init ls]
                
                (if (empty? ls)
                  
                  init
                  
                  (let [fst (first ls)
                        rst (rest rst)
                        result (f init fst)]

                    (reduce f result rest))))

              (reduce + '(1 2 3)))
   
   [:h2 "Searching and Sorting"]
   [:h3 "Bubble Sort"]
   [:h3 "Quicksort"]

   '(add-code quicksort

              (defn quicksort [ls]
                
                (if (empty? ls)
                  
                  ls
                  
                  (let [pivot (first ls)
                        
                        rst (rest ls)

                        smaller? (fn [val]
                                   (>= pivot val))

                        larger? (fn [val]
                                  (< pivot val))]
                    
                    (concat (quicksort (filter smaller? rst))
                            pivot
                            (quicksort (filter larger? rst)))))))
   [:h2 "Conclusion"]
   
   [:h1 "The Meta-Circular Interpreter"]
   [:h2 "The Eval / Apply Loop"]
   [:h2 "S-Expressions"]
   [:h2 "Symbols & Environment"]
   [:h2 "Conditionals"]
   [:h2 "Functions"]
   [:p "Why would we even do this?"]
   
   [:h1 "The Y-Combinator"]
   [:p (str (repeat 500  "-"))]))


(def page
  (compile-page
   
   
   [:h1 "Introduction"]

   
   [:h1 "Syntax"]
   [:p "The history and mystery"]
   [:h2 "Arithmetic"]

   '(add-code arithmetic

              (+ 1 3 (* 8 4)))
   
   [:h2 "Lists"]
   [:h2 "Conditionals"]
   [:h2 "Bindings"]

   '(add-code let

              (let [x (+ 1 4)
                    y (- x 3)
                    z (- x y)]
                (if (zero? 1)
                  (+ x y z)
                  (* x y z))))

   [:h2 "Factorial"]

   '(add-code factorial

   
              (defn factorial [n]
                (if (zero? n)
                  1
                  (* n (factorial (- n 1)))))

              (factorial 3))

   [:p (str (repeat 1000  "-"))]))

;;##########################################################################
;; Mounting Elements
;;##########################################################################

(defn get-app-element []
  (gdom/getElement "app"))


(defn mount [el]
  (rdom/render [main page] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)
    (add-scroll-triggers! page)
    ;;(add-click!)
    ))

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
