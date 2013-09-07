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

(defn transform-ingress-lanes [old-lanes safety-fn]
  (par-map-merge q/harvest-ingress-lane
                 (par-map-merge q/advance-cars-in-lane
                                (par-map-merge q/mark-ripe
                                               (par-map-merge q/ch->lane old-lanes)))
                 i/directions-index i/ingress-lane-state-index safety-fn))

(defn transform-egress-lanes [old-lanes]
  (par-map-merge q/harvest-egress-lane
                 (par-map-merge q/advance-cars-in-lane
                                (par-map-merge q/mark-ripe
                                               (par-map-merge q/ch->lane old-lanes)))
                 i/directions-index i/egress-lane-state-index))

(defn genesis! [old-i-lanes old-e-lanes old-lights safety-fn]
  (pprint (map (fn [[k v]] {k (:state v)}) old-lights))
  (pprint (map (fn [[k v]] {(:street/tag k) (:state v)}) old-i-lanes))
  (pprint (map (fn [[k v]] {(:street/tag k) (:state v)}) old-e-lanes))

  (let [new-i-lanes (future (transform-ingress-lanes old-i-lanes (partial safety-fn old-i-lanes old-lights)))
        new-e-lanes (future (transform-egress-lanes old-e-lanes))
        new-lights  (future (par-map-merge q/next-light-state old-lights))]
;    (Thread/sleep 1000)
    (recur @new-i-lanes @new-e-lanes @new-lights safety-fn)))

(def safety-f
  (partial r/safe-to-go?
           i/lane-index
           i/lanes-rules-substitution-index
           i/atomic-rule-index
           i/lane-var-catalog))

