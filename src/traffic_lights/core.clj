(ns traffic-lights.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.tools.logging :refer [info]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]
            [dire.core :as d]
            [traffic-lights.queue :as q]
            [traffic-lights.index :as i]))

(defn parallel-map-merge [f state]
  (apply merge (pmap (fn [[k v]] {k (f v)}) state)))

(defn build-light-state-machine [x]
  {:state (:state-diff (first x))
   :fns (mapcat q/light-transition->fns x)})

(def light-state-machines
  (parallel-map-merge build-light-state-machine i/traffic-light-index))

(defn drive [old-lanes old-lights]
  (let [new-lanes  (future (parallel-map-merge q/next-lane-state old-lanes))
        new-lights (future (parallel-map-merge q/next-light-state old-lights))]
    (Thread/sleep 1000)
    (recur @new-lanes @new-lights)))

(d/with-pre-hook! #'drive
  (fn [lanes lights]
    (pprint (parallel-map-merge :state lights))))

