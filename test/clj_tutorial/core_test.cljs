(ns clj-tutorial.core-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     
     [clj-tutorial.interpreter.tracer :refer [gen-evaluation-stream]]))

(deftest multiply-test
  (is (= (* 1 2) (* 1 2))))

(deftest multiply-test-2
  (is (= (* 75 10) (* 10 75))))
