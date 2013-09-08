(ns traffic-lights.core
  (:require [clojure.tools.logging :refer [info]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.queue :as q]
            [traffic-lights.index :as i]
            [traffic-lights.rules :as r]))

(defn maph [f coll & args]
  (apply merge (map (fn [[k v]] {k (apply f k v args)}) coll)))

(defn build-light-state-machine [x]
  {:state (:state-diff (first x))
   :fns (mapcat q/light-transition->fns x)})

(def light-state-machines
  (apply merge (map (fn [{:keys [intersection state-seq]}]
                {intersection (build-light-state-machine state-seq)})
              i/traffic-light-catalog)))

(defn transform-ingress-lanes [old-i-lanes old-e-lanes safety-fn]
  (->> old-i-lanes
       (maph #(q/ch->lane %2))
       (maph #(q/mark-ripe %2))
       (maph #(q/advance-cars-in-lane %2))
       (maph #(q/harvest-ingress-lane %2 i/directions-index old-e-lanes safety-fn))))

(defn transform-egress-lanes [old-lanes]
  (->> old-lanes
       (maph #(q/ch->lane %2))
       (maph #(q/mark-ripe %2))
       (maph #(q/advance-cars-in-lane %2))
       (maph #(q/harvest-egress-lane %2 i/directions-index old-lanes))))

(defn transform-lights [old-lights]
  (->> old-lights
       (maph #(q/next-light-state %2))))

(defn log-lanes! [idx]
  (pprint
   (map (fn [[k v]]
          {[(:intersection/of k) (:street/tag k)]
           [(:state v) (:channel v)]}) idx)))

(defn genesis! [old-i-lanes old-e-lanes old-lights safety-fn]
  (log-lanes! old-i-lanes)
  (log-lanes! old-e-lanes)
  (pprint (maph #(:state %2) old-lights))
  (let [new-e-lanes (transform-egress-lanes old-e-lanes)
        new-lights  (transform-lights old-lights)
        new-i-lanes (transform-ingress-lanes
                     old-i-lanes old-e-lanes
                     (partial safety-fn old-i-lanes old-lights))]
    (recur new-i-lanes new-e-lanes new-lights safety-fn)))

(def safety-f
  (partial r/safe-to-go?
           i/lane-index
           i/lanes-rules-substitution-index
           i/atomic-rule-index
           i/lane-var-catalog))

