(ns traffic-lights.core
  (:require [clojure.pprint :refer [pprint]]
            [traffic-lights.transform :as t]
            [traffic-lights.boot :as b]
            [traffic-lights.protocols :as p]
            [traffic-lights.rules :as r]
            [traffic-lights.queue :as q]
            [traffic-lights.util :refer [maph]]))

(def schema
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(def connections
  (read-string (slurp (clojure.java.io/resource "connections-schema.edn"))))

(def drivers
  (read-string (slurp (clojure.java.io/resource "drivers.edn"))))

(def directions
  (read-string (slurp (clojure.java.io/resource "directions.edn"))))

(def spec-source (concat schema connections drivers directions))

(def storage (p/memory-storage spec-source))

(def safety-fn (partial r/safe-to-go? storage))

(def lights (map (partial b/boot-light storage) (p/intersections storage)))

(def ingress-lanes (map b/boot-lane (p/ingress-lanes storage)))

(def egress-lanes (map b/boot-lane (p/egress-lanes storage)))

(defn log! [idx]
  (pprint (maph :state idx)))

(defn genesis! [old-lights old-ingress-lanes old-egress-lanes safety-f]
  (log! old-lights)
  (log! old-ingress-lanes)
  (log! old-egress-lanes)
  
  (let [new-lights        (t/transform-lights old-lights)
        new-egress-lanes  (t/transform-egress-lanes old-egress-lanes)
        new-ingress-lanes (t/transform-ingress-lanes
                           old-ingress-lanes
                           old-egress-lanes
                           (partial safety-fn old-ingress-lanes old-lights))]
    (recur new-lights new-ingress-lanes new-egress-lanes safety-f)))

;(q/put-into-ch (:channel (second (second i/ingress-lane-state-index))) {:id "Mike" :len 3 :buf 0})

;(genesis! lights i/ingress-lane-state-index i/egress-lane-state-index safety-fn)

