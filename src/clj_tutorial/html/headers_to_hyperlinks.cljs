
(ns ^:figwheel-hooks clj-tutorial.html.headers-to-hyperlinks
  (:require
   [clj-tutorial.html.d3 :as d3]
   [clj-tutorial.html.scrolling :refer [set-scroll-trigger]]))

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







(defn hiccup?
  [div]
  (vector? div))

(defn populate-outline-with-headers!
  [text]
  (doseq [t text]
    
    (when (hiccup? t)
      
      (let [id (:id (second t))
            
            element (d3/select-by-id (str "a-" id))

            change-color (fn [obj turn-on? color]
                           (-> obj
                               (d3/classed "highlighted" turn-on?)
                               (d3/style "color" color)))

            f (fn [_]
                
                (when-let [prior (d3/select ".highlighted")]
                  (change-color prior false sidebar-link-color))

                (change-color element true "white" ))]
        
        (set-scroll-trigger id f)))))
