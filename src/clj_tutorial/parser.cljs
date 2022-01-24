
(ns ^:figwheel-hooks clj-tutorial.parser
  (:require

   [clj-tutorial.utils :as u]
   [edamame.core :refer [parse-string]]

   [clojure.repl :refer [source]]
   [clojure.pprint :refer [pprint]]))


(defn gen-token-id
  "Creates a unique token id that will
   link each Clojure element's AST to its
   html representation."
  []
  (gensym "code_"))


;; Parsing
(defn add-token-id-to-metadata
  [form]
  (u/append-metadata form {:token-id (gen-token-id)}))

(defn add-positions-to-metadata
  [form loc]
  (u/append-metadata form loc))

(defn parse-postprocessing

  "Decorates parsed Clojure token with
   position and identification information.

   Wraps token in Constant if it doesn't support
   metadata."
  
  [{:keys [obj loc]}]

  (-> obj
      (u/wrap-if-not-IMeta)
      (add-token-id-to-metadata)
      (add-positions-to-metadata loc)))

(defn string->code
  "Converts a string into Clojure code,
   with id and position information.

   This output will be turned into ASTs
   and Hiccup."
  [s]
  (parse-string s {:postprocess parse-postprocessing
                   :var true
                   :quote true
                   :fn true
                   :regex true
                   :all true
                   :deref (fn [form]
                            (let [m (with-meta
                                      'deref
                                      {:row 1 :id (gen-token-id)}) ]
                              
                              (list m form)))}))
