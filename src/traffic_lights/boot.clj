(ns traffic-lights.boot
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [traffic-lights.resolve :as r]
            [traffic-lights.queue :as q]
            [traffic-lights.util :refer [maph]])
  (:import [java.util.concurrent LinkedBlockingQueue]))

(defn boot-lane [id]
  {:lane id :state [] :channel (LinkedBlockingQueue. 1)})

(defn light-init-state [intx intx-reg-idx group-idx face-idx]
  {:diff (r/resolve-initial-light intx intx-reg-idx group-idx face-idx)
   :ticks 0})

(defn light-subsequent-states [intx intx-reg-idx group-idx]
  (r/resolve-light-sequence intx intx-reg-idx group-idx))

(defn to-light-sm [light]
  {:state (:diff (first light))
   :fns (mapcat q/light-transition->fns light)})

(defn form-full-light-seq [intx intx-reg-idx group-idx face-idx]
  {intx (cons (light-init-state intx intx-reg-idx group-idx face-idx)
              (light-subsequent-states intx intx-reg-idx group-idx))})

(defn boot-light [intx-reg-idx schedule-idx face-idx intx]
  (maph to-light-sm (form-full-light-seq intx intx-reg-idx schedule-idx face-idx)))

