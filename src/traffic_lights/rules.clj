(ns traffic-lights.rules
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [traffic-lights.index :as i]
            [clojure.pprint :refer [pprint]]))

(defn eval-atom [{:keys [src dst yield] :as rule} mapping]
  (assoc rule
    :src (mapping src)
    :dst (mapping dst)
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

