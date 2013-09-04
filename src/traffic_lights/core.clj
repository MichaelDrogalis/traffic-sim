(ns traffic-lights.core
  (:require [clojure.tools.logging :refer [info]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.queue :as q]
            [traffic-lights.index :as i]))

(defn par-map-merge [f state & args]
  (apply merge (map (fn [[k v]] {k (apply f k v args)}) state)))

(defn build-light-state-machine [_ x]
  {:state (:state-diff (first x))
   :fns (mapcat q/light-transition->fns x)})

(def light-state-machines
  (par-map-merge build-light-state-machine i/traffic-light-index))

(defn loud-lights! [lights]
  (pprint (par-map-merge :state lights)))

(defn loud-lanes! [lanes]
  (pprint (par-map-merge :state lanes)))

(defn transform-lanes [old-lanes]
  (par-map-merge q/harvest-lane
                 (par-map-merge q/advance-cars-in-lane
                                (par-map-merge q/mark-ripe
                                               (par-map-merge q/ch->lane old-lanes)))
                 i/directions-index i/lane-index))

(defn genesis! [old-lanes old-lights]
  (pprint old-lanes)
  (let [new-lanes (transform-lanes old-lanes)
        new-lights (par-map-merge q/next-light-state old-lights)]
    (Thread/sleep 2000)
    (recur new-lanes new-lights)))

(def car {:id "Mike" :len 5 :buf 3})
(q/enqueue-into-ch (:channel (second (first i/lane-state-index))) car)

(genesis! i/lane-state-index [])

