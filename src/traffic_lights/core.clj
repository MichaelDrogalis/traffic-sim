(ns traffic-lights.core
  (:require [clojure.pprint :refer [pprint]]
            [traffic-lights.index :as i]
            [traffic-lights.transform :as t]
            [traffic-lights.boot :as b]
            [traffic-lights.protocols :as p]
            [traffic-lights.rules :as r]
            [traffic-lights.util :refer [maph]]))

(def storage (p/memory-storage i/spec-source))

(def indexed-boot-light (partial b/boot-light storage))

(def safety-fn (partial r/safe-to-go? storage ingress-lanes))

(def lights
  (apply merge (map indexed-boot-light (keys i/intx-registration-index))))

(defn log-lights! [idx]
  (pprint (maph :state idx)))

(defn log-lanes! [idx]
  (pprint idx))

(defn genesis! [old-lights old-ingress-lanes old-egress-lanes safety-f]
  (log-lights! old-lights)
  (log-lanes! old-ingress-lanes)
  (log-lanes! old-egress-lanes)
  
  (let [new-lights        (t/transform-lights old-lights)
        new-egress-lanes  (t/transform-egress-lanes old-egress-lanes)
        new-ingress-lanes (t/transform-ingress-lanes
                                old-ingress-lanes
                                old-egress-lanes
                                (partial safety-fn old-ingress-lanes old-lights))]
    (Thread/sleep 1000)
    (recur new-lights new-ingress-lanes new-egress-lanes safety-f)))

;;; (genesis! lights i/ingress-lane-state-index i/egress-lane-state-index safety-fn)

