(ns traffic-lights.directions
  (:require [traffic-lights.util :refer [only]]))

(defn matching-driver? [driver candidate]
  (= (:directions/for candidate) driver))

(defn matching-src? [src candidate]
  (= (:directions/src candidate) src))

(defn matching-candidate? [id src candidate]
  (and (matching-driver? id candidate)
       (matching-src? src candidate)))

(defn find-dst [d-catalog]
  (fn [id src]
    (let [match-fn (partial matching-candidate? id src)]
      (:directions/dst (only (filter match-fn d-catalog))))))

