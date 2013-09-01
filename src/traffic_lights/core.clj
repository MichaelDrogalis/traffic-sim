(ns traffic-lights.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.tools.logging :refer [info]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.queue :as q]
            [traffic-lights.index :as i]))

(defn parallel-map-merge [f state]
  (apply merge (pmap (fn [[k v]] {k (f v)}) state)))

(defn build-light-state-machine [x]
  {:state (:state-diff (first x))
   :fns (mapcat q/light-transition->fns x)})

(def light-state-machines
  (parallel-map-merge build-light-state-machine i/traffic-light-index))

(defn loud-lights! [lights]
  (pprint (parallel-map-merge :state lights)))

(defn loud-lanes! [lanes]
  (pprint (parallel-map-merge :state lanes)))

(defn drive [old-lanes old-lights]
  (pprint old-lanes)
  (let [new-lanes  (future (parallel-map-merge q/next-lane-state
                                               (parallel-map-merge q/ch->lane old-lanes)))
        new-lights (future (parallel-map-merge q/next-light-state old-lights))]
    (Thread/sleep 1000)
    (recur @new-lanes @new-lights)))

;(future (drive i/lane-state-index []))
;(q/enqueue-into-ch (:channel (second (first i/lane-state-index))) {:id "Mike" :len 5})


