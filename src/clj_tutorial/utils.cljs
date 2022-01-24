(ns ^:figwheel-hooks clj-tutorial.utils
  (:require

   [clojure.walk :as w]
   [clojure.repl :refer [source]]
   [clojure.pprint :refer [pprint]]))



;;#####################################################################
;; Constants
;;#####################################################################

;; Constants don't support metadata, so we wrap them in their own record.
(defrecord Constant [val])

(defn constant?
  [form]
  (instance? Constant form))

(defn wrap-if-not-IMeta
  [form]
  (if (satisfies? IMeta form)
    form
    (->Constant form)))


;;#####################################################################
;; Dispatch
;;#####################################################################

(defn form->type
  "Used as dispatch function for
   AST and HTML parser multimethods."
  [form _]
  (cond
    (constant? form)  :constant
    (symbol? form)    :symbol
    (list? form)      :list
    (vector? form)    :vector
    (map? form)       :map
    (set? form)       :set))






;;#####################################################################
;; Walking
;;#####################################################################

(defn postwalk
  "A version of postwalk that preserves
   metadata."
  [f form]
  (let [f (fn [form]
            (let [form* (f form)]
              (if-let [m (meta form)]
                (with-meta form* (merge (meta form*) m))
                form*)))]
    (w/walk (partial postwalk f) f form)))

;;#####################################################################
;; General
;;#####################################################################

(defn append-metadata
  [form maps]
  (vary-meta form merge maps))
