(ns traffic-lights.succession
  (:require [traffic-lights.queue :as q]
            [traffic-lights.util :refer [maph]]))

(defn transform-ingress-lanes [old-i-lanes old-e-lanes d-fn s-fn]
  (->> old-i-lanes
       (maph q/ch->lane)
       (maph q/mark-ripe)
       (maph q/advance-cars-in-lane)
       (maph #(q/harvest-ingress-lane % d-fn old-e-lanes s-fn))))

(defn transform-egress-lanes [old-lanes d-fn]
  (->> old-lanes
       (maph q/ch->lane)
       (maph q/mark-ripe)
       (maph q/advance-cars-in-lane)
       (maph #(q/harvest-egress-lane % d-fn old-lanes))))

(defn transform-lights [old-lights]
  (->> old-lights
       (maph q/next-light-state)))

(defn transform-world-fn [directions-fn safety-fn]
  (fn [{:keys [lights egress ingress]}]
    {:lights (transform-lights lights)
     :ingress (transform-ingress-lanes ingress egress directions-fn (partial safety-fn ingress lights))
     :egress (transform-egress-lanes egress directions-fn)}))

