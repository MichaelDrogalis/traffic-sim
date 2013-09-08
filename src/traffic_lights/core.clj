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

(defn transform-ingress-lanes [old-lanes safety-fn]
  (->> old-lanes
       (maph #(q/ch->lane %2))
       (maph #(q/mark-ripe %2))
       (maph #(q/advance-cars-in-lane %2))
       (maph #(q/harvest-ingress-lane %2 i/directions-index old-lanes safety-fn))))

(defn transform-lights [old-lights]
  (->> old-lights
       (maph #(q/next-light-state %2))))

(defn genesis! [old-i-lanes old-e-lanes old-lights safety-fn]
  (pprint old-i-lanes)
  (let [new-i-lanes (transform-ingress-lanes old-i-lanes (partial safety-fn old-i-lanes old-lights))
        new-lights  (transform-lights old-lights)]
    (recur new-i-lanes old-e-lanes new-lights safety-fn)))

(def safety-f
  (partial r/safe-to-go?
           i/lane-index
           i/lanes-rules-substitution-index
           i/atomic-rule-index
           i/lane-var-catalog))

(q/enqueue-into-ch (:channel (nth i/ingress-lane-state-catalog 2)) {:id "Mike" :len 1 :buf 0})

(genesis! i/ingress-lane-state-index
          i/egress-lane-state-index
          light-state-machines
          safety-f)

