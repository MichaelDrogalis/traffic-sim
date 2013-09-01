(ns traffic-lights.core
  (:require [clojure.tools.logging :refer [info]]
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

(defn genesis! [old-lanes old-lights]
  (let [new-lanes  (future (-> old-lanes
                               (partial parallel-map-merge q/ch->lane)
                               (partial parallel-map-merge q/mark-ripe)
                               (partial parallel-map-merge q/advance-cars-in-lane)
                               (partial parallel-map-merge q/harvest-lane)))
        new-lights (future (parallel-map-merge q/next-light-state old-lights))]
    (Thread/sleep 1000)
    (recur @new-lanes @new-lights)))

