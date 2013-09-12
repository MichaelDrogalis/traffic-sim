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

(def ingress-lanes (map b/boot-lane (keys i/ingress-lane-state-index)))

(def egress-lanes (map b/boot-lane (keys i/egress-lane-state-index)))

(def safety-fn (partial r/safe-to-go? storage ingress-lanes))

(def lights
  (apply merge (map indexed-boot-light (keys i/intx-registration-index))))

(defn log! [idx]
  (pprint (maph :state idx)))

(defn genesis! [old-lights old-ingress-lanes old-egress-lanes safety-f]
  (log! old-lights)
  (log! old-ingress-lanes)
  (log! old-egress-lanes)
  
  (let [new-lights (t/transform-lights old-lights)
        new-ingress-lanes (t/transform-ingress-lanes old-ingress-lanes (partial safety-fn old-ingress-lanes old-lights))
        new-egress-lanes (t/transform-egress-lanes old-egress-lanes)]
    (Thread/sleep 1000)
    (recur new-lights new-ingress-lanes new-egress-lanes safety-f)))

(genesis! lights ingress-lanes egress-lanes safety-fn)
