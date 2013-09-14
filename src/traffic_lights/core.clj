(ns traffic-lights.core
  (:require [clojure.pprint :refer [pprint]]
            [traffic-lights.boot :as b]
            [traffic-lights.succession :as s]
            [traffic-lights.protocols :as p]
            [traffic-lights.rules :as r]
            [traffic-lights.queue :as q]
            [traffic-lights.util :refer [maph index-by-lane-id]]))

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

(def dir-fn (r/find-dst directions))

(def safety-fn (partial r/safe-to-go? storage))

(def transform-world (s/transform-world-fn dir-fn safety-fn))

(def lights (into {} (map (partial b/boot-light storage) (p/intersections storage))))

(def ingress-lanes 
  (->> storage
       (p/ingress-lanes)
       (map index-by-lane-id)
       (into {})
       (maph b/boot-lane)))

(def egress-lanes
  (->> storage
       (p/egress-lanes)
       (map index-by-lane-id)
       (into {})
       (maph b/boot-lane)))

(defn log! [idx]
  (pprint (maph :state idx)))

(defn genesis! [lights ingress egress t-fn]
  (log! lights)
  (log! ingress)
  (log! egress)
  (let [successor (t-fn {:lights lights :ingress ingress :egress egress})]
    (recur (:lights successor) (:ingress successor) (:egress successor) t-fn)))

;(q/put-into-ch (:channel (second (second ingress-lanes))) {:id "Mike" :len 3 :buf 0})

;(genesis! lights ingress-lanes egress-lanes transform-world)

