(ns traffic-lights.core
  (:require [clojure.pprint :refer [pprint]]
            [traffic-lights.boot :as b]
            [traffic-lights.succession :as s]
            [traffic-lights.protocols :as p]
            [traffic-lights.rules :as r]
            [traffic-lights.directions :as d]
            [traffic-lights.util :refer [maph index-by-quad]]))

(def intersections
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(def connections
  (read-string (slurp (clojure.java.io/resource "connections-schema.edn"))))

(def drivers
  (read-string (slurp (clojure.java.io/resource "drivers.edn"))))

(def directions
  (read-string (slurp (clojure.java.io/resource "directions.edn"))))

(def schema (concat intersections connections drivers))

(def storage (p/memory-storage schema))

(def dir-fn (d/find-dst directions))

(def safety-fn (partial r/safe-to-go? storage))

(def transform-world (s/transform-world-fn dir-fn safety-fn))

(def lights (b/lights storage))

(def ingress-lanes (b/ingress-lanes storage))

(def egress-lanes (b/egress-lanes storage))

(defn log! [idx]
  (pprint (maph :state idx)))

(defn genesis! [lights ingress egress t-fn]
  (log! lights)
  (log! ingress)
  (log! egress)
  (let [successor (t-fn {:lights lights :ingress ingress :egress egress})]
    (recur (:lights successor) (:ingress successor) (:egress successor) t-fn)))

