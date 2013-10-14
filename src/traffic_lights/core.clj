(ns traffic-lights.core
  (:require [clojure.pprint :refer [pprint]]
            [traffic-lights.boot :as b]
            [traffic-lights.succession :as s]
            [traffic-lights.protocols :as p]
            [traffic-lights.rules :as r]
            [traffic-lights.directions :as d]
            [traffic-lights.util :refer [maph index-by-quad] :as u]))

(def intersections
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(def connections
  (read-string (slurp (clojure.java.io/resource "connections-schema.edn"))))

(def weights
  (read-string (slurp (clojure.java.io/resource "weighted-directions.edn"))))

(def schema (concat intersections connections))

(def storage (p/memory-storage schema))

(def dir-fn (d/weighted-directions storage weights))

(def safety-fn (r/safe-to-go? storage))

(def transform-world (s/transform-world-fn dir-fn safety-fn))

(def lights (b/lights storage))

(def ingress-lanes (b/ingress-lanes storage))

(def egress-lanes (b/egress-lanes storage))

(def starting-state {:lights lights :ingress ingress-lanes :egress egress-lanes})

(def queue (agent starting-state))

(defn genesis! [snapshot t-fn queue]
  (let [successor (t-fn snapshot)]
    (send-off queue (constantly successor))
    (Thread/sleep 500)
    (recur successor t-fn queue)))

(defn start-sim []
  (future (genesis! starting-state transform-world queue)))

