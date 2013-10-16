(ns traffic-lights.core
  (:require [clojure.pprint :refer [pprint]]
            [traffic-lights.boot :as b]
            [traffic-lights.succession :as s]
            [traffic-lights.protocols :as p]
            [traffic-lights.rules :as r]
            [traffic-lights.directions :as d]
            [traffic-lights.queue :as q]
            [traffic-lights.util :refer [maph index-by-quad] :as u]))

(def intersections
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(def connections
  (read-string (slurp (clojure.java.io/resource "connections-schema.edn"))))

(def weights
  (read-string (slurp (clojure.java.io/resource "weighted-directions.edn"))))

(def schema (concat intersections connections))

(def storage (p/memory-storage schema))

(def internal-fn (d/weighted-internal-directions storage weights))

(def external-fn (d/weighted-external-directions storage weights))

(def safety-fn (r/safe-to-go? storage))

(def transform-world (s/transform-world-fn internal-fn external-fn safety-fn))

(def lights (b/lights storage))

(def ingress-lanes (b/ingress-lanes storage))

(def egress-lanes (b/egress-lanes storage))

(def starting-state {:lights lights :ingress ingress-lanes :egress egress-lanes})

(def queue (agent starting-state))

(defn genesis! [snapshot t-fn queue]
  (let [successor (t-fn snapshot)]
    (send-off queue (constantly successor))
    (Thread/sleep 3000)
    (recur successor t-fn queue)))

(def walnut-11-east-in
  {:intersection/of ["11th Street" "Walnut Street"]
   :street/name "Walnut Street"
   :street/tag "east"
   :lane/name "in-2"})

(defn start-sim []
  (q/put-into-ch (:channel (ingress-lanes walnut-11-east-in))
                 {:id "Mike" :buf 0 :len 1})
  (future (genesis! starting-state transform-world queue)))

