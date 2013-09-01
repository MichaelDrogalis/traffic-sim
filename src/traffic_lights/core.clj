(ns traffic-lights.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.tools.logging :refer [info]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.queue :as q]
            [traffic-lights.index :as i]))

(defn parallel-map-merge [f state]
  (apply merge (pmap (fn [[k v]] {k (f v)}) state)))

(def light-state-machines
 (apply merge
        (map
         (fn [[intx state-seq]]
           {intx {:state (:state-diff (first state-seq))
                  :fns (mapcat q/light-transition->fns state-seq)}})
         i/traffic-light-index)))

(defn drive [old-lanes old-lights]
  (let [new-lanes  (future (parallel-map-merge q/next-lane-state old-lanes))
        new-lights (future (parallel-map-merge q/next-light-state old-lights))]
    (Thread/sleep 1000)
    (recur @new-lanes @new-lights)))

