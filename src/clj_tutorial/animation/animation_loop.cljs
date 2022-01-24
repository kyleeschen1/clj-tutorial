
(ns ^:figwheel-hooks clj-tutorial.animation.animation_loop
  (:require
   [clj-tutorial.utils :as u]

   [cljs.core.async :refer [chan close! >! <! go-loop]]
   
   [clojure.repl :refer [source]]
   [clojure.pprint :refer [pprint]] ))



;;################################################################################
;; Async
;;################################################################################

;; https://gist.github.com/swannodette/5882703

(defn timeout [ms]
  (let [c (chan)]
    (js/setTimeout (fn [] (close! c)) ms)
    c))

(defn on-end
  [f sel]
  (.on sel "end" f))

(defn block

  "Blocks animation loop
   until CSS animations comeplete."

  [f & args]
  
  (let [c (chan)
        evaluated (apply f args)]

    (on-end (fn []
              (close! c))
            evaluated)
    
    c))




;;################################################################################
;; Animation
;;################################################################################

(defn animate
  [& args]
  :form)

;;################################################################################
;; Run Animation Loop
;;################################################################################

(def loop-count
  (atom 0))

(def paused?
  (atom false))


(defn continue?

  [max-count state]

  (swap! loop-count inc)
  
  (and (not @paused?)
       (< @loop-count  max-count)))


(defn run-eval-loop

  [evaluation-stream config]
  
  (try
    
    (go-loop [[state & states] evaluation-stream]

      (when (continue? 100 state)

        ;; (<! (block animate state config))
        (<! (timeout (:pause-time @config)))

        (pprint state)
        
        (recur states)))

    (catch js/Error. e
      
      (js/console.log "Evaluator Error")
      
      (js/console.log e))))


