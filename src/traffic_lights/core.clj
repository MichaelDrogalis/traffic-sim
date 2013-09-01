(ns traffic-lights.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.tools.logging :refer [info]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.queue :as q]
            [traffic-lights.index :as i]))

(def light-state-machines
 (apply merge
        (map
         (fn [[intx state-seq]]
           {intx {:state (:state-diff (first state-seq))
                  :fns (mapcat q/light-transition->fns state-seq)}})
         i/traffic-light-index)))

(defn drive [old-lanes old-lights]
  (pprint (map (fn [[k v]] {k (:state v)}) old-lights))
  (let [new-lanes (apply merge (pmap (fn [[k v]] {k (q/next-lane-state v)}) old-lanes))
        new-lights (apply merge (pmap (fn [[k v]] {k (q/next-light-state v)}) old-lights))]
    (Thread/sleep 1000)
    (recur new-lanes new-lights)))

