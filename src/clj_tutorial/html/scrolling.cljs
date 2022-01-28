
(ns ^:figwheel-hooks clj-tutorial.html.scrolling
  (:require [cljsjs.waypoints]))


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
