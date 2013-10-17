(ns traffic-lights.directions
  (:require [traffic-lights.protocols :as p]
            [traffic-lights.util :refer [only quad]]))

(defn weighted-rand-nth [weights]
  (rand-nth (mapcat (fn [[k v]] (take v (repeat k))) weights)))

(defn weight-quad [lane-id]
  {(dissoc lane-id :lane/weight) (:lane/weight lane-id)})

(defn matching-driver? [driver candidate]
  (= (:directions/for candidate) driver))

(defn matching-src? [src candidate]
  (= (:directions/src candidate) src))

(defn matching-candidate? [id src candidate]
  (and (matching-driver? id candidate)
       (matching-src? src candidate)))

(defn matching-quad? [id candidate]
  (= (quad id) (quad candidate)))

(defn find-dst [d-catalog]
  (fn [id src]
    (let [match-fn (partial matching-candidate? id (quad src))]
      (:directions/dst (only (filter match-fn d-catalog))))))

(defn find-weights [d-catalog lane-id]
  (let [match-fn (partial matching-quad? lane-id)]
    (only (filter match-fn d-catalog))))

(defn weighted-directions [weights links]
  (weighted-rand-nth
   (apply merge
          (map weight-quad
               (map (partial find-weights weights)
                    links)))))

(defn weighted-internal-directions [storage weights]
  (fn [_ lane-id]
    (weighted-directions weights (p/internal-links storage lane-id))))

(defn weighted-external-directions [storage weights]
  (fn [_ lane-id]
    (weighted-directions weights (p/external-links storage lane-id))))

