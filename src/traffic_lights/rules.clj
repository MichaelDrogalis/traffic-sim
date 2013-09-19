(ns traffic-lights.rules
  (:require [clojure.set :refer [subset?]]
            [traffic-lights.protocols :as p]
            [traffic-lights.util :refer [getx only quad] :as u]))

(defn lane-clear?
  ([lane-idx src]
     (empty? (:state (lane-idx (quad src)))))
  ([lane-idx src dst]
     (let [head-car (first (:state (lane-idx (quad src))))]
       (not= (:dst head-car) (quad dst)))))

(defn matches-src? [target-src rule]
  (= (quad (:src rule)) target-src))

(defn matches-dst? [target-dst rule]
  (= (quad (:dst rule)) target-dst))

(defn matches-light? [light-state rule]
  (subset? light-state (into #{} (:light rule))))

(defn matches-yield? [lane-idx rule]
  (every? true? (map (partial apply lane-clear? lane-idx) (:yield rule))))

(defn safe-to-go? [storage]
  (fn [old-lanes old-lights src dst]
    (let [lane-id (quad src)
          lane-idx (into {} (map u/index-by-quad (p/lanes storage)))
          intx (:intersection/of lane-id)
          face (:street.lane.install/light (lane-idx lane-id))
          light (getx (:state (old-lights intx)) face)
          rules (p/resolve-rules storage (lane-idx lane-id))
          match-f (every-pred (partial matches-src? src)
                              (partial matches-dst? dst)
                              (partial matches-light? light)
                              (partial matches-yield? old-lanes))]
      (not (empty? (filter match-f rules))))))

