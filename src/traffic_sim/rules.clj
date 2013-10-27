(ns traffic-sim.rules
  (:require [clojure.set :refer [subset?]]
            [traffic-sim.protocols :as p]
            [traffic-sim.util :refer [getx quad] :as u]))

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

(defn index-quads [lanes]
  (into {} (map u/index-by-quad lanes)))

(defn all-rules-for-src [storage src]
  (let [lane-idx (index-quads (p/lanes storage))]
    (p/resolve-rules storage (lane-idx (quad src)))))

(defn light-for-src [storage old-lights src]
  (let [lane-idx (index-quads (p/lanes storage))
        face (:street.lane.install/light (lane-idx (quad src)))]
    (getx (:state (old-lights (:intersection/of src))) face)))

(defn safe-to-go? [storage]
  (fn [old-lanes old-lights src dst]
    (let [all-rules (all-rules-for-src storage src)
          light (light-for-src storage old-lights src)
          match-f (every-pred (partial matches-src? src)
                              (partial matches-dst? dst)
                              (partial matches-light? light)
                              (partial matches-yield? old-lanes))]
      (not (empty? (filter match-f all-rules))))))

