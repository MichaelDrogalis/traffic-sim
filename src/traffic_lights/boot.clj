(ns traffic-lights.boot
  (:require [clojure.algo.generic.functor :refer [fmap]])
  (:import [java.util.concurrent LinkedBlockingQueue]))

(defn build-lane [id]
  {:lane id :state [] :channel (LinkedBlockingQueue. 1)})

(defn build-light-for-schedule [schedule light-catalog]
  (fmap #(:light-face/init (light-face-index %)) (:schedule/substitute schedule)))

(defn build-light-sequence [schedule]
  (:schedule/sequence (light-group-schedule-index schedule)))

(defn initial-light-state [intx]
  {:state-diff (-> (intx-registration-index intx)
                   :intersection.install/schedule
                   light-group-schedule-index
                   (build-light-for-schedule light-face-index))
   :ticks 0})

(defn build-light-state-machine [x]
  {:state (:state-diff (first x))
   :fns (mapcat q/light-transition->fns x)})

(def light-state-machines
  (apply merge (map (fn [{:keys [intersection state-seq]}]
                {intersection (build-light-state-machine state-seq)})
              i/traffic-light-catalog)))

