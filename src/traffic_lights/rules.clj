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

(defn without-ident [x]
  (dissoc x :street.lane.install/ident))

(defn compile-src [vtable rule]
  (getx vtable (:src rule)))

(defn compile-dst [vtable rule]
  (getx vtable (:dst rule)))

(defn compile-yield-atom
  ([vtable src]
     [(getx vtable src)])
  ([vtable src dst]
     [(getx vtable src)
      (getx vtable dst)]))

(defn compile-yield [vtable rule]
  (map (partial apply compile-yield-atom vtable) (:yield rule)))

(defn compile-atom [vtable rule]
  (assoc rule
    :src (compile-src vtable rule)
    :dst (compile-dst vtable rule)
    :yield (compile-yield vtable rule)))

(defn local-var-index [lane var-catalog]
  (var-catalog (:intersection/of lane)))

(defn rule-set-name [lane]
  (:street.lane.install/rules lane))

(defn eval-local-lane-subs [lane locals]
  (fmap #(getx locals %) (:street.lane.install/substitute lane)))

(defn compile-binders [registered-rules evaled-lane-subs]
  (map #(assoc % :lane.rules/substitute
               (merge (:lane.rules/substitute %) evaled-lane-subs))
       registered-rules))

(defn eval-atomic-rule [atomic-index compiled-binders]
  (map #(compile-atom (:lane.rules/substitute %)
                      (atomic-index (:lane.rules/register %))) compiled-binders))

(defn eval-all-atomic-rules [lane sub-index atomic-index var-catalog]
  (let [locals-index (local-var-index lane var-catalog)
        rule-set (rule-set-name lane)
        bound-rules (getx sub-index rule-set)
        evaled-lane-subs (eval-local-lane-subs lane locals-index)
        compiled-binders (compile-binders bound-rules evaled-lane-subs)
        evaled-atomic-rules (eval-atomic-rule atomic-index compiled-binders)]
    evaled-atomic-rules))

(defn matching-paths [atomic-rules target-src target-dst]
  (filter
   (fn [{:keys [src dst]}]
     (and (= (without-ident (first src)) target-src)
          (= (without-ident (first dst)) target-dst)))
   atomic-rules))

(defn matching-lights [atomic-rules light-state]
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

(defn safe-to-go? [lane-idx rule-sub-idx atomic-rule-idx var-catalog old-lanes light-state-idx src dst]
  (let [lane-id (dissoc src :street.lane.install/type)
        intx (:intersection/of lane-id)
        face (:street.lane.install/light (lane-idx lane-id))
        rules (eval-all-atomic-rules (lane-idx lane-id) rule-sub-idx atomic-rule-idx var-catalog)
        applicable-rules (matching-paths rules src dst)
        light-state (getx (:state (light-state-idx intx)) face)
        matching (matching-lights applicable-rules light-state)]
    (and (not (empty? matching)) (all-lanes-clear? old-lanes matching))))

