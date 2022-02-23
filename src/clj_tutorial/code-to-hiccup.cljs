

(ns ^:figwheel-hooks clj-tutorial.code-to-hiccup
  (:require
   [clj-tutorial.utils :as u]))

;;#######################################################################
;; Code  -> HTML
;;#######################################################################

(def code->hiccup)

;;==============================================
;; Styles
;;==============================================

(def form->id 
  (comp :token-id meta))

(defn add-id

  ([form]
   (add-id form {}))

  ([form style]
   (assoc style :id (form->id form))))

(defn add-x-y [x y]
  
  (let [fs 18
        x-scale 0.66
        y-scale 1.25]
    
    {:style {:position "relative"
             :font-family "Courier"
             :font-size (str fs "px")}
     
     :y (* y y-scale fs)
     :x (* x x-scale fs)}))

(defn get-bracket-styles

  "Generates style map for the
   brackets of a given form."
  
  [form]
  
  (let [{:keys [row col end-row end-col]} (meta form)
        
        op (add-x-y col row)
        cl (add-x-y (dec end-col) end-row)

        [op cl] (map (fn [s]
                       (assoc s :id
                               (form->id form)))
                     [op cl])]

    [op cl]))

;;==============================================
;; Helpers
;;==============================================

(defn atom->hiccup

  "Transforms an atomic Clojure
   element into a hiccup component."

  [form {:keys [row col]}]
  
  (let [style (add-id form (add-x-y col row))]
    
    [:text style (str form)]))

(defn coll->hiccup
  
  "Transforms a collection into nested
   hiccup components."
  
  [form env op cl]
  
  (let [[op-style cl-style] (get-bracket-styles form)]
      
      (-> [:g (add-id form)]
          (conj [:text.opening op-style op] )
          (into  (map code->hiccup form (repeat env)))
          (conj [:text.closing cl-style cl]))))


;;-------------------------------------
;; Implementation
;;-------------------------------------

(defmulti code->hiccup u/form->type)

(defmethod code->hiccup :default
  [form env]
  (atom->hiccup form (meta form)))

(defmethod code->hiccup :constant
  [form env]
  (atom->hiccup (:val form) (meta form)))

(defmethod code->hiccup :symbol
  [form env]
  (atom->hiccup form (meta form)))

(defmethod code->hiccup :list
  [form env]
  (coll->hiccup form env "(" ")"))

(defmethod code->hiccup :vector
  [form env]
  (coll->hiccup form env "[" "]"))

(defmethod code->hiccup :set
  [form env]
  (coll->hiccup form env "#{" "}"))

(defmethod code->hiccup :map
  [form env]
  (let [seq-map (apply concat (seq form))
        seq-map (with-meta seq-map (meta form))] 
    (coll->hiccup seq-map env "{" "}")))
