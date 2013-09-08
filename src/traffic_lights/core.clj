(ns traffic-lights.core
  (:require [clojure.tools.logging :refer [info]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.queue :as q]
            [traffic-lights.index :as i]
            [traffic-lights.rules :as r]))

(defn build-light-state-machine [x]
  {:state (:state-diff (first (:state-seq x)))
   :fns (mapcat q/light-transition->fns (:state-seq x))})

(def light-state-machines
  (map build-light-state-machine i/traffic-light-catalog))

(defn transform-ingress-lanes [old-lanes safety-fn]
  (map q/harvest-ingress-lane
       (map q/advance-cars-in-lane
            (map q/mark-ripe
                 (map q/ch->lane old-lanes)))
       i/directions-index i/ingress-lane-state-catalog safety-fn))

(defn transform-egress-lanes [old-lanes]
  (map q/harvest-egress-lane
       (map q/advance-cars-in-lane
            (map q/mark-ripe
                 (map q/ch->lane old-lanes)))
       i/directions-index i/egress-lane-state-catalog))

(defn genesis! [old-i-lanes old-e-lanes old-lights safety-fn]
  (let [new-i-lanes (future (transform-ingress-lanes old-i-lanes (partial safety-fn old-i-lanes old-lights)))
        new-e-lanes (future (transform-egress-lanes old-e-lanes))
        new-lights  (future (par-map-merge q/next-light-state old-lights))]
    (recur @new-i-lanes @new-e-lanes @new-lights safety-fn)))

(def safety-f
  (partial r/safe-to-go?
           i/lane-index
           i/lanes-rules-substitution-index
           i/atomic-rule-index
           i/lane-var-catalog))

