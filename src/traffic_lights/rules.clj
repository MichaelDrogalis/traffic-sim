(ns traffic-lights.rules
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [traffic-lights.index :as i]
            [clojure.pprint :refer [pprint]]))

(defn eval-atom [{:keys [src dst yield] :as rule} mapping]
  (assoc rule
    :src (mapping src)
    :dst (mapping dst)
    :yield (map (fn [[src dst]] [(first (mapping src)) (first (mapping dst))]) yield)))

;;; 1. Obtain all lane data
;;; 2. Get the rule set name for the lane
;;; 3. Get all of the registered rules for the rule set
;;; 4. Get the intersection-var map
;;; 5. Evaluate the lane's local substitution map
;;; 6. Evaluate each registered rule's substitution map
;;; 7. Evalulate each atomic rule's src, dst, and yield
(defn eval-all-atomic-rules [lane-index sub-index atomic-index lane-id]
  (let [lane (get lane-index lane-id)
        rule-set (:street.lane.install/rules lane)
        var-mapping (i/lane-var-catalog (:intersection/of lane))
        registered-rules (get sub-index rule-set)
        evaluated-lane-var-mapping (fmap #(var-mapping %) (:street.lane.install/substitute lane))
        evaluated-registered-rules (map #(assoc % :lane.rules/substitute
                                                (merge (:lane.rules/substitute %) evaluated-lane-var-mapping))
                                        registered-rules)
        evaled-atomic-rules (map #(eval-atom (atomic-index (:lane.rules/register %))
                                                    (:lane.rules/substitute %)) evaluated-registered-rules)]
    evaled-atomic-rules))

(pprint (eval-all-atomic-rules i/lane-index i/lanes-rules-subtitution-index i/atomic-rule-index (ffirst i/lane-index)))

