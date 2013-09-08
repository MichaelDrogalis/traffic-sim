(ns traffic-lights.rules
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]))

(defn getx
  "Like two-argument get, but throws an exception if the key is not found."
  [m k]
  (let [e (get m k ::sentinel)]
    (if-not (= e ::sentinel)
      e
      (throw (ex-info "Missing required key" {:map m :key k})))))

(defn to-index [coll k]
  (reduce (fn [all x] (conj all {(k x) x})) {} coll))

(defn without-ident [x]
  (dissoc x :street.lane.install/ident))

(defn eval-yield
  ([mapping src]
     [(getx mapping src)])
  ([mapping src dst]
     [(getx mapping src)
      (getx mapping dst)]))

(defn eval-atom [{:keys [src dst yield] :as rule} mapping]
  (assoc rule
    :src (getx mapping src)
    :dst (getx mapping dst)
    :yield (map (partial apply eval-yield mapping) yield)))

(defn local-var-index [lane var-catalog]
  (var-catalog (:intersection/of lane)))

(defn rule-set-name [lane]
  (:street.lane.install/rules lane))

(defn eval-local-lane-subs [lane locals]
  (fmap #(getx locals %) (:street.lane.install/substitute lane)))

(defn eval-binders [registered-rules evaled-lane-subs]
  (map #(assoc % :lane.rules/substitute
               (merge (:lane.rules/substitute %) evaled-lane-subs))
       registered-rules))

(defn eval-atomic-rule [atomic-index evaled-binders]
  (map #(eval-atom (atomic-index (:lane.rules/register %))
                   (:lane.rules/substitute %)) evaled-binders))

(defn eval-all-atomic-rules [lane sub-index atomic-index var-catalog]
  (let [locals-index (local-var-index lane var-catalog)
        rule-set (rule-set-name lane)
        registered-rules (getx sub-index rule-set)
        evaled-lane-subs (eval-local-lane-subs lane locals-index)
        evaled-binders (eval-binders registered-rules evaled-lane-subs)
        evaled-atomic-rules (eval-atomic-rule atomic-index evaled-binders)]
    evaled-atomic-rules))

(defn relevant-rules [atomic-rules target-src target-dst]
  (filter
   (fn [{:keys [src dst]}]
     (and (= (map without-ident src) target-src)
          (= (map without-ident dst) target-dst)))
   atomic-rules))

(defn matching-lights [atomic-rules light-state]
  (prn "Light is: " light-state)
  (prn "Rules are: " atomic-rules)
  (filter #(subset? light-state (into #{} (:light %))) atomic-rules))

(defn lane-clear?
  ([lane-state-index src]
     (empty? (:state (lane-state-index src))))
  ([lane-state-index src dst]
     (let [head-car (first (:state (lane-state-index src)))]
       (when (nil? (:dst head-car)) (throw (ex-info "Head car dst not implemented." {:car head-car})))
       (not= (:dst head-car) dst))))

(defn all-lanes-clear? [lane-state-index lanes]
  (every? #(lane-clear? lane-state-index %) lanes))

(defn safe-to-go? [lane-idx rule-sub-idx atomic-rule-idx var-catalog old-lanes light-state-catalog src dst]
  (prn "~~~")
  (let [lane-id (dissoc src :street.lane.install/type)
        light-state-index (to-index light-state-catalog :intersection/of)
        _   (prn light-state-index)
        light-state (light-state-index lane-id)
        rules (eval-all-atomic-rules (lane-idx lane-id) rule-sub-idx atomic-rule-idx var-catalog)
        applicable-rules (relevant-rules rules src dst)
        matching (matching-lights applicable-rules light-state)]
    (and (not (empty? matching)) (all-lanes-clear? old-lanes matching))))

