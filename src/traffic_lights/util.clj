(ns traffic-lights.util
  (:require [clojure.core.reducers :as r]))

(defn getx
  "Like two-argument get, but throws an exception if the key is not found."
  [m k]
  (let [e (get m k ::sentinel)]
    (if-not (= e ::sentinel)
      e
      (throw (ex-info "Missing required key" {:map m :key k})))))

(defn only
  "Like first, but throws unless exactly one item."
  [coll]
  (assert (not (next coll)))
  (if-let [result (first coll)]
    result
    (assert false)))

(defn maph [f coll & args]
  (into {} (r/reduce (fn [a k v] (conj a {k (apply f v args)})) [] coll)))

(defn quad [lane]
  (select-keys lane [:intersection/of :street/name :street/tag :street.lane.install/name]))

(defn index-by-quad [x]
  {(quad x) x})

(defn find-lane [target lanes]
  (only (filter (fn [x] (= (quad (:lane x)) target)) lanes)))

