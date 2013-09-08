(ns traffic-lights.rules
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.util :refer [getx without-ident]]))

(defn local-var-index [lane var-catalog]
  (var-catalog (:intersection/of lane)))

(defn rule-set-name [lane]
  (:street.lane.install/rules lane))

(defn find-atom-from-binder [atom-index binder]
  (atom-index (:lane.rules/register binder)))

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
    :src   (compile-src vtable rule)
    :dst   (compile-dst vtable rule)
    :yield (compile-yield vtable rule)))

(defn compile-locals [vtable lane]
  (fmap #(getx vtable %) (:street.lane.install/substitute lane)))

(defn compile-binder [vtable binder]
  (update-in binder [:lane.rules/substitute] (partial fmap (partial getx vtable))))

(defn compile-binders [vtable binders]
  (map (partial compile-binder vtable) binders))

(defn compile-atoms [compiled-binders atomic-index]
  (map #(compile-atom (:lane.rules/substitute %)
                      (find-atom-from-binder atomic-index %))
       compiled-binders))

(defn compile-rules [lane sub-index atomic-index var-catalog]
  (let [locals-index (local-var-index lane var-catalog)
        rule-set (rule-set-name lane)
        bound-rules (getx sub-index rule-set)]
    (-> locals-index
        (compile-locals lane)
        (compile-binders bound-rules)
        (compile-atoms atomic-index))))

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
        rules (compile-rules (lane-idx lane-id) rule-sub-idx atomic-rule-idx var-catalog)
        applicable-rules (matching-paths rules src dst)
        light-state (getx (:state (light-state-idx intx)) face)
        matching (matching-lights applicable-rules light-state)]
    (and (not (empty? matching)) (all-lanes-clear? old-lanes matching))))

