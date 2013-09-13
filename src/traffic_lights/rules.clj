(ns traffic-lights.rules
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.protocols :as p]
            [traffic-lights.util :refer [getx] :as u]))

(defn lane-clear?
  ([lane-idx src]
     (empty? (:state (lane-idx src))))
  ([lane-idx src dst]
     (let [head-car (first (:state (lane-idx src)))]
       (when (nil? (:dst head-car))
         (throw (ex-info "Head car dst not implemented." {:car head-car})))
       (not= (:dst head-car) dst))))

(defn matches-src? [target-src rule]
  (= (u/without-ident (first (:src rule))) target-src))

(defn matches-dst? [target-dst rule]
  (= (u/without-ident (first (:dst rule))) target-dst))

(defn matches-light? [light-state rule]
  (subset? light-state (into #{} (:light rule))))

(defn matches-yield? [lane-idx rule]
  (map (partial apply lane-clear? lane-idx) (:yield rule)))

(defn safe-to-go? [storage old-lanes old-lights src dst]
  (let [lane-id (dissoc src :street.lane.install/type)
        lane-idx (into {} (map u/index-by-lane-id (p/lanes storage)))
        intx (:intersection/of lane-id)
        face (:street.lane.install/light (lane-idx lane-id))
        light (getx (:state (old-lights intx)) face)
        rules (p/resolve-rules storage (lane-idx lane-id))
        match-f (every-pred (partial matches-src? src)
                            (partial matches-dst? dst)
                            (partial matches-light? light)
                            (partial matches-yield? lane-idx))]
    (not (empty? (filter match-f rules)))))

