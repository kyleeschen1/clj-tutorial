
(ns ^:figwheel-hooks clj-tutorial.html.d3
  (:require [cljsjs.d3]))


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
