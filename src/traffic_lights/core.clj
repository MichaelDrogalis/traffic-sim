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
    (Thread/sleep 400)
    (recur successor t-fn queue)))

(def walnut-juniper-north-in
  {:intersection/of ["Juniper Street" "Walnut Street"]
   :street/name "Juniper Street"
   :street/tag "north"
   :lane/name "in"})

(defn start-sim []
  (doseq [n (range 10)]
    (q/put-into-ch (:channel (ingress-lanes walnut-juniper-north-in))
                   {:id (str n) :buf 5 :len 1}))
  (future (genesis! starting-state transform-world queue)))

