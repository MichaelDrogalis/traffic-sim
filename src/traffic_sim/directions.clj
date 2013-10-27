(ns traffic-sim.directions
  (:require [traffic-sim.protocols :as p]
            [traffic-sim.util :refer [only quad]]))

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
  (if (not (empty? links))
    (let [resolved-lanes (map (partial find-weights weights) links)]
      (weighted-rand-nth
       (apply merge (map weight-quad resolved-lanes))))
    nil))

(defn weighted-internal-directions [storage weights]
  (fn [_ lane-id]
    (let [links (p/internal-links storage lane-id)]
      (weighted-directions weights links))))

(defn weighted-external-directions [storage weights]
  (fn [_ lane-id]
    (let [links (p/external-links storage lane-id)]
      (weighted-directions weights links))))

