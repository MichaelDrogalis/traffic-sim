(ns traffic-lights.core
  (:require [clojure.tools.logging :refer [info]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.index :as i]
            [traffic-lights.rules :as r]
            [traffic-lights.util :refer [maph]]))

(defn log-lanes! [idx]
  (pprint
   (map (fn [[k v]]
          {[(:intersection/of k) (:street/tag k)]
           [(:state v) (:channel v)]}) idx)))

(defn log-lights! [idx]
 (pprint (maph :state idx)))

(defn genesis! [old-i-lanes old-e-lanes old-lights safety-fn]
  (log-lanes! old-i-lanes)
  (log-lanes! old-e-lanes)
  (log-lights! old-lights)
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

