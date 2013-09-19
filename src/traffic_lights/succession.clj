(ns traffic-lights.succession
  (:require [traffic-lights.queue :as q]
            [traffic-lights.util :refer [maph]]))

(defn transform-ingress-lanes [old-i-lanes old-e-lanes d-fn s-fn]
  (->> old-i-lanes
       (maph q/mark-ripe)
       (maph q/advance-cars-in-lane)
       (maph #(q/harvest-ingress-lane % d-fn old-e-lanes s-fn))))

(defn transform-egress-lanes [old-lanes d-fn]
  (->> old-lanes
       (maph q/mark-ripe)
       (maph q/advance-cars-in-lane)
       (maph #(q/harvest-egress-lane % d-fn old-lanes))))

(defn transform-lights [old-lights]
  (->> old-lights
       (maph q/next-light-state)))

(defn transform-world-fn [d-fn s-fn]
  (fn [{:keys [lights egress ingress]}]
    (let [pulled-ingress (future (transform-ingress-lanes ingress egress d-fn (partial s-fn ingress lights)))
          pulled-egress (future (transform-egress-lanes egress d-fn))
          new-lights (future (transform-lights lights))
          new-ingress @pulled-ingress
          new-egress @pulled-egress]
      {:lights  @new-lights
       :ingress (maph #(q/ch->lane % d-fn) new-ingress)
       :egress  (maph #(q/ch->lane % d-fn) new-egress)})))

