(ns traffic-lights.rules
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.resolve :as r]
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
  (= (u/without-ident (first (:src rule)) target-src)))

(defn matches-dst? [target-dst rule]
  (= (u/without-ident (first (:dst rule)) target-dst)))

(defn matches-light? [light-state rule]
  (subset? light-state (into #{} (:light rule))))

(defn matches-yield? [lane-idx rule]
  (map (partial apply lane-clear? lane-idx) (:yield rule)))

(defn safe-to-go? [lane-idx binder-idx rule-idx vtable old-lanes light-state-idx src dst]
  (let [lane-id (dissoc src :street.lane.install/type)
        intx (:intersection/of lane-id)
        face (:street.lane.install/light (lane-idx lane-id))
        light (getx (:state (light-state-idx intx)) face)
        rules (r/resolve-all-rules (lane-idx lane-id) binder-idx rule-idx vtable)
        match-f (every-pred (partial matches-src? src)
                            (partial matches-dst? dst)
                            (partial matches-light? light)
                            (partial matches-yield? lane-idx))]
    (not (empty? (filter match-f rules)))))

