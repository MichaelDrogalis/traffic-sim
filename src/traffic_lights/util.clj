(ns traffic-lights.util
  (:require [clojure.core.reducers :as r]))

(defn getx
  "Like two-argument get, but throws an exception if the key is not found."
  [m k]
  (let [e (get m k ::sentinel)]
    (if-not (= e ::sentinel)
      e
      (throw (ex-info "Missing required key" {:map m :key k})))))

(defn maph [f coll & args]
  (into {} (r/reduce (fn [a k v] (conj a {k (apply f v args)})) [] coll)))

(defn without-ident [x]
  (dissoc x :street.lane.install/ident))

(defn lane-id [lane]
  (select-keys lane [:intersection/of :street/name :street/tag :street.lane.install/name]))

(defn index-by-lane-id [x]
  {(lane-id x) x})

(defn find-lane [target lanes]
  (first (filter (fn [x] (= (lane-id (:lane x)) target)) lanes)))

