(ns traffic-lights.rules
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.set :refer [subset?]]
            [traffic-lights.index :as i]
            [clojure.pprint :refer [pprint]]))

(defn without-ident [x]
  (dissoc x :street.lane.install/ident))

(defn eval-atom [{:keys [src dst yield] :as rule} mapping]
  (assoc rule
    :src (without-ident (first (mapping src)))
    :dst (without-ident (first (mapping dst)))
    :yield (map (fn [[src dst]] [(first (mapping src)) (first (mapping dst))]) yield)))

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
     (empty? (:state (get i/lane-state-index src))))
  ([lane-state-index src dst]
     (let [head-car (first (:state (get i/lane-state-index src)))]
       (not= (:dst head-car) dst))))

(defn all-lanes-clear? [lane-state-index lanes]
  (every? #(lane-clear? lane-state-index %) lanes))

(defn safe-to-go? [rule-sub-idx atomic-rule-idx light-state-index src dst]
  (let [light-state (light-state-index src)
        rules (eval-all-atomic-rules src rule-sub-idx atomic-rule-idx)
        applicable-rules (relevant-rules rules src dst)
        matching (matching-lights applicable-rules light-state)]
    (all-lanes-clear? matching)))

