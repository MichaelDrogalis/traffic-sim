(ns traffic-lights.resolve
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.util :refer [getx] :as u]))

(defn resolve-src [vtable rule]
  (getx vtable (:src rule)))

(defn resolve-dst [vtable rule]
  (getx vtable (:dst rule)))

(defn resolve-yield-case
  ([vtable src]
     [(getx vtable src)])
  ([vtable src dst]
     [(getx vtable src)
      (getx vtable dst)]))

(defn resolve-yield [vtable rule]
  (map (partial apply resolve-yield-case vtable) (:yield rule)))

(defn resolve-rule [vtable rule]
  (assoc rule
    :src   (resolve-src vtable rule)
    :dst   (resolve-dst vtable rule)
    :yield (resolve-yield vtable rule)))

(defn resolve-locals [vtable lane]
  (fmap #(getx vtable %) (:street.lane.install/substitute lane)))

(defn resolve-binder [vtable binder]
  (update-in binder [:lane.rules/substitute] (partial fmap (partial getx vtable))))

(defn resolve-binders [vtable binders]
  (map (partial resolve-binder vtable) binders))

(defn resolve-rules [resolved-binders rule-index]
  (map #(resolve-rule (:lane.rules/substitute %)
                      (u/find-rule-from-binder rule-index %))
       resolved-binders))

(defn resolve-all-rules [lane binder-idx rule-idx vtable]
  (let [local-vtable (u/local-var-index lane vtable)
        rule-set (u/rule-set-name lane)
        bound-rules (getx binder-idx rule-set)]
    (-> local-vtable
        (resolve-locals lane)
        (resolve-binders bound-rules)
        (resolve-rules rule-idx))))

