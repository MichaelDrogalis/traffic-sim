(ns traffic-lights.core
  (:require [clojure.tools.logging :refer [info]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.queue :as q]
            [traffic-lights.index :as i]
            [traffic-lights.rules :as r]))

(defn par-map-merge [f state & args]
  (apply merge (pmap (fn [[k v]] {k (apply f k v args)}) state)))

(defn build-light-state-machine [_ x]
  {:state (:state-diff (first x))
   :fns (mapcat q/light-transition->fns x)})

(def light-state-machines
  (par-map-merge build-light-state-machine i/traffic-light-index))

(defn loud-lights! [lights]
  (pprint (par-map-merge :state lights)))

(defn loud-lanes! [lanes]
  (pprint (par-map-merge :state lanes)))

(defn transform-lanes [old-lanes safety-fn]
  (par-map-merge q/harvest-lane
                 (par-map-merge q/advance-cars-in-lane
                                (par-map-merge q/mark-ripe
                                               (par-map-merge q/ch->lane old-lanes)))
                 i/directions-index i/lane-state-index safety-fn))

(defn genesis! [old-lanes old-lights safety-fn]
  (pprint (map (fn [[k v]] {k (:state v)}) old-lights))
  (pprint (map (fn [[k v]] {k (:state v)}) old-lanes))
  (let [new-lanes  (future (transform-lanes old-lanes (partial safety-fn old-lanes old-lights)))
        new-lights (future (par-map-merge q/next-light-state old-lights))]
    (Thread/sleep 1000)
    (recur @new-lanes @new-lights safety-fn)))

(def safety-f
  (partial r/safe-to-go?
           i/lane-index
           i/lanes-rules-substitution-index
           i/atomic-rule-index))

(q/enqueue-into-ch (:channel (second (first i/lane-state-index))) {:id "Mike" :len 1 :buf 0})

(genesis! i/lane-state-index light-state-machines safety-f)

