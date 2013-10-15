(ns traffic-lights.succession
  (:require [traffic-lights.queue :as q]
            [traffic-lights.util :refer [maph]]))

(defn transform-ingress-lanes [old-i-lanes old-e-lanes d-fn s-fn]
  (->> old-i-lanes
       (maph q/mark-ripe)
       (maph q/advance-cars-in-lane)
       (maph #(q/harvest-ingress-lane % d-fn old-e-lanes s-fn))))

(defn transform-egress-lanes [old-e-lanes old-i-lanes d-fn]
  (->> old-e-lanes
       (maph q/mark-ripe)
       (maph q/advance-cars-in-lane)
       (maph #(q/harvest-egress-lane % d-fn old-i-lanes))))

(defn transform-lights [old-lights]
  (->> old-lights
       (maph q/next-light-state)))

(defn transform-world-fn [internal-fn external-fn s-fn]
  (fn [{:keys [lights egress ingress]}]
    (let [pulled-ingress (future (transform-ingress-lanes ingress egress internal-fn (partial s-fn ingress lights)))
          pulled-egress (future (transform-egress-lanes egress ingress external-fn))
          new-lights (future (transform-lights lights))
          new-ingress @pulled-ingress
          new-egress @pulled-egress]
      {:lights  @new-lights
       :ingress (maph #(q/ch->lane % internal-fn) new-ingress)
       :egress  (maph #(q/ch->lane % external-fn) new-egress)})))

