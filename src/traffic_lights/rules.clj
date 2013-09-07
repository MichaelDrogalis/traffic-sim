(ns traffic-lights.rules
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.set :refer [subset?]]
            [traffic-lights.index :as i]
            [clojure.pprint :refer [pprint]]))

(defn getx
  "Like two-argument get, but throws an exception if the key is
   not found."
  [m k]
  (let [e (get m k ::sentinel)]
    (if-not (= e ::sentinel)
      e
      (throw (ex-info "Missing required key" {:map m :key k})))))

(defn without-ident [x]
  (dissoc x :street.lane.install/ident))

(defn eval-yield
  ([mapping src]
     [(first (getx mapping src))])
  ([mapping src dst]
     [(first (getx mapping src))
      (first (getx mapping dst))]))

(defn eval-atom [{:keys [src dst yield] :as rule} mapping]
  (assoc rule
    :src (without-ident (first (getx mapping src)))
    :dst (without-ident (first (getx mapping dst)))
    :yield (map (partial apply eval-yield mapping) yield)))

(defn local-var-index [lane var-catalog]
  (var-catalog (:intersection/of lane)))

(defn rule-set-name [lane]
  (:street.lane.install/rules lane))

(defn eval-local-lane-subs [lane locals]
  (fmap #(locals %) (:street.lane.install/substitute lane)))

(defn eval-binders [registered-rules evaled-lane-subs]
  (map #(assoc % :lane.rules/substitute
               (merge (:lane.rules/substitute %) evaled-lane-subs))
       registered-rules))

(defn eval-atomic-rule [atomic-index evaled-binders]
  (map #(eval-atom (atomic-index (:lane.rules/register %))
                   (:lane.rules/substitute %)) evaled-binders))

(defn eval-all-atomic-rules [lane sub-index atomic-index]
  (let [locals-index (local-var-index lane i/lane-var-catalog)
        rule-set (rule-set-name lane)
        registered-rules (get sub-index rule-set)
        evaled-lane-subs (eval-local-lane-subs lane locals-index)
        evaled-binders (eval-binders registered-rules evaled-lane-subs)
        evaled-atomic-rules (eval-atomic-rule atomic-index evaled-binders)]
    evaled-atomic-rules))

(defn relevant-rules [atomic-rules target-src target-dst]
  (filter
   (fn [{:keys [src dst]}]
     (and (= src target-src) (= dst target-dst)))
   atomic-rules))

(defn matching-lights [atomic-rules light-state]
  (filter #(subset? light-state (into #{} (:light %))) atomic-rules))

(defn lane-clear?
  ([lane-state-index src]
     (empty? (:state (lane-state-index src))))
  ([lane-state-index src dst]
     (let [head-car (first (:state (lane-state-index src)))]
       (not= (:dst head-car) dst))))

(defn all-lanes-clear? [lane-state-index lanes]
  (every? #(lane-clear? lane-state-index %) lanes))

(use 'clojure.pprint)

(defn safe-to-go? [lane-idx rule-sub-idx atomic-rule-idx old-lanes light-state-index src dst]
  (let [lane-id (dissoc src :street.lane.install/type)
        light-state (light-state-index lane-id)
        rules (eval-all-atomic-rules (lane-idx lane-id) rule-sub-idx atomic-rule-idx)
        applicable-rules (relevant-rules rules src dst)
        matching (matching-lights applicable-rules light-state)]
    (all-lanes-clear? old-lanes matching)))

