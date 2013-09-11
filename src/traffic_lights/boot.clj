(ns traffic-lights.boot
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [traffic-lights.resolve :as r]
            [traffic-lights.queue :as q]
            [traffic-lights.protocols :as p]
            [traffic-lights.util :refer [maph]])
  (:import [java.util.concurrent LinkedBlockingQueue]))

(defn boot-lane [id]
  {:lane id :state [] :channel (LinkedBlockingQueue. 1)})

(defn light-init-state [schema intx]
  {:diff (p/initial-light schema intx)
   :ticks 0})

(defn light-subsequent-states [schema intx]
  (p/light-sequence schema intx))

(defn to-light-sm [light]
  {:state (:diff (first light))
   :fns (mapcat q/light-transition->fns light)})

(defn form-full-light-seq [schema intx]
  {intx (cons (light-init-state schema intx)
              (light-subsequent-states schema intx))})

(defn boot-light [schema intx]
  (maph to-light-sm (form-full-light-seq schema intx)))

