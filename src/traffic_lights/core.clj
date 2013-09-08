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
  (map #(q/harvest-ingress-lane % i/directions-index i/ingress-lane-state-index safety-fn)
       (map q/advance-cars-in-lane
            (map q/mark-ripe
                 (map q/ch->lane old-lanes)))))

(defn genesis! [old-i-lanes old-e-lanes old-lights safety-fn]
  (pprint old-i-lanes)
  (let [new-i-lanes (transform-ingress-lanes old-i-lanes (partial safety-fn old-i-lanes old-lights))]
    (recur new-i-lanes old-e-lanes old-lights safety-fn)))

(def safety-f
  (partial r/safe-to-go?
           i/lane-index
           i/lanes-rules-substitution-index
           i/atomic-rule-index
           i/lane-var-catalog))

(q/enqueue-into-ch (:channel (nth i/ingress-lane-state-catalog 2)) {:id "Mike" :len 1 :buf 0})

(genesis! i/ingress-lane-state-catalog
          i/egress-lane-state-catalog
          light-state-machines
          safety-f)

