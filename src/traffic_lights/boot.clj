(ns traffic-lights.boot
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [traffic-lights.queue :as q]
            [traffic-lights.protocols :as p]
            [traffic-lights.util :refer [maph] :as u])
  (:import [java.util.concurrent LinkedBlockingQueue]))

(defn boot-lane [id]
  {:lane id :state [] :channel (LinkedBlockingQueue. 1)})

(defn light-init-state [storage intx]
  {:diff (p/initial-light storage intx)
   :ticks 0})

(defn light-subsequent-states [storage intx]
  (p/light-sequence storage intx))

(defn to-light-sm [light]
  {:state (:diff (first light))
   :fns (mapcat q/light-transition->fns light)})

(defn form-full-light-seq [storage intx]
  {intx (cons (light-init-state storage intx)
              (light-subsequent-states storage intx))})

(defn boot-light [storage intx]
  (maph to-light-sm (form-full-light-seq storage intx)))

(defn load-lanes [storage f]
  (->> storage
       (f)
       (map u/index-by-quad)
       (into {})
       (maph boot-lane)))

(defn ingress-lanes [storage]
  (load-lanes storage p/ingress-lanes))

(defn egress-lanes [storage]
  (load-lanes storage p/egress-lanes))

(defn lights [storage]
  (into {} (map (partial boot-light storage) (p/intersections storage))))

