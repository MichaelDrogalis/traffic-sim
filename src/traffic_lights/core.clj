(ns traffic-lights.core
  (:require [clojure.pprint :refer [pprint]]
            [traffic-lights.boot :as b]
            [traffic-lights.succession :as s]
            [traffic-lights.protocols :as p]
            [traffic-lights.rules :as r]
            [traffic-lights.directions :as d]
            [traffic-lights.util :refer [maph index-by-quad]]))

(def intersections
  (read-string (slurp (clojure.java.io/resource "ring-schema.edn"))))

(def connections
  (read-string (slurp (clojure.java.io/resource "ring-connections.edn"))))

(def drivers
  (read-string (slurp (clojure.java.io/resource "drivers.edn"))))

(def directions
  (read-string (slurp (clojure.java.io/resource "ring-directions.edn"))))

(def schema (concat intersections connections drivers))

(def storage (p/memory-storage schema))

(def dir-fn (d/find-dst directions))

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

(def chestnut-10-north-in
  {:intersection/of ["10th Street" "Chestnut Street"]
   :street/name "10th Street"
   :street/tag "north"
   :lane/name "in"})

(def chestnut-10-east-out
  {:intersection/of ["10th Street" "Chestnut Street"]
   :street/name "Chestnut Street"
   :street/tag "east"
   :lane/name "out"})

(traffic-lights.queue/put-into-ch
 (:channel (get ingress-lanes chestnut-10-north-in))
 {:id "Mike" :len 1 :buf 0})

(traffic-lights.queue/put-into-ch
 (:channel (get egress-lanes chestnut-10-east-out))
 {:id "Dorrene" :len 1 :buf 0})

(future (genesis! starting-state transform-world queue))

