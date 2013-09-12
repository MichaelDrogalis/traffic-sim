(ns traffic-lights.boot
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [traffic-lights.resolve :as r]
            [traffic-lights.queue :as q]
            [traffic-lights.protocols :as p]
            [traffic-lights.util :refer [maph]])
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

