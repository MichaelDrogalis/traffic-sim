(ns traffic-lights.memory
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.util :refer [getx only]]))

(defn resolve-schedule [intersection]
  (:intersection.install/schedule intersection))

(defn resolve-substitute [template]
  (:schedule/substitute template))

(defn resolve-sequence [template]
  (:schedule/sequence template))

(defn resolve-lane-var [lane]
  (:street.lane.install/ident lane))

(defn resolve-intersection [lane]
  (:intersection/of lane))

(defn resolve-intersection-id [registration]
  (:intersection/ident registration))

(defn resolve-street-name [lane]
  (:street/name lane))

(defn resolve-tag [lane]
  (:street/tag lane))

(defn resolve-lane-name [lane]
  (:street.lane.install/name lane))

(defn resolve-rule-set-name [lane]
  (:street.lane.install/rules lane))

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

(defn resolve-locals [lane vtable]
  (fmap #(getx vtable %) (:street.lane.install/substitute lane)))

(defn resolve-binder [vtable binder]
  (update-in binder [:lane.rules/substitute] (partial fmap (partial getx vtable))))

(defn resolve-binders [binders vtable]
  (map (partial resolve-binder vtable) binders))

(defn intersection-registration-index [schema]
  (filter #(contains? % :intersection/ident) schema))

(defn intersection-index [schema]
  (map resolve-intersection-id (intersection-registration-index schema)))

(defn lane-index [schema]
  (filter #(contains? % :intersection/of) schema))

(defn ingress-lane-index [schema]
  (filter #(= :ingress (:street.lane.install/type %)) (lane-index schema)))

(defn egress-lane-index [schema]
  (filter #(= :egress (:street.lane.install/type %)) (lane-index schema)))

(defn links-index [schema]
  (filter #(contains? % :connection/src) schema))

(defn binder-index [schema]
  (filter #(contains? % :lane.rules/of) schema))

(defn rule-index [schema]
  (filter #(contains? % :rule/ident) schema))

(defn light-group-index [schema]
  (filter #(contains? % :schedule/ident) schema))

(defn light-face-index [schema]
  (filter #(contains? % :light-face/init) schema))

(defn var->lane-index [schema intx]
  (let [match-f #(= intx (resolve-intersection %))
        matches (filter match-f (lane-index schema))]
    (fmap only (group-by :street.lane.install/ident matches))))

(defn find-intersection [vtable intersection]
  (only (filter #(= (resolve-intersection-id %) intersection) vtable)))

(defn find-rule-by-binder [rule-index binder]
  (only (filter #(= (:lane.rules/register binder) (:rule/ident %)) rule-index)))

(defn matches-intersection? [quad candidate]
  (= (resolve-intersection candidate) (resolve-intersection quad)))

(defn matches-street-name? [quad candidate]
  (= (resolve-street-name candidate) (resolve-street-name quad)))

(defn matches-tag? [quad candidate]
  (= (resolve-tag candidate) (resolve-tag quad)))

(defn matches-lane-name? [quad candidate]
  (= (resolve-lane-name candidate) (resolve-lane-name quad)))

(defn find-lane [vtable quad]
  (let [match-f (every-pred (partial matches-intersection? quad)
                            (partial matches-street-name? quad)
                            (partial matches-tag? quad)
                            (partial matches-lane-name? quad))]
    (only (filter match-f vtable))))

(defn find-rule [vtable rule]
  (only (filter #(= (get % :rule/ident) rule) vtable)))

(defn find-light-init [vtable light]
  (only (filter #(= (get % :light-face/ident) light) vtable)))

(defn find-light-template [vtable schedule]
  (only (filter #(= (get % :schedule/ident) schedule) vtable)))

(defn match-binders [vtable binder]
  (filter #(= binder (:lane.rules/of %)) vtable))

(defn match-rules [rules binders]
  (map #(find-rule rules (:lane.rules/register %)) binders))

(defn match-links [schema quad]
  (let [links (links-index schema)]
    (map :connection/dst (filter #(= quad (:connection/src %)) links))))

(defn resolve-rules [rule-index resolved-binders]
  (let [resolution #(resolve-rule (:lane.rules/substitute %)
                                  (find-rule-by-binder rule-index %))]    
    (map resolution resolved-binders)))

(defn resolve-light-init [vtable template]
  (fmap #(:light-face/init (find-light-init vtable %)) template))

(defn resolve-initial-light [schema intx]
  (->> intx
       (find-intersection (intersection-registration-index schema))
       (resolve-schedule)
       (find-light-template (light-group-index schema))
       (resolve-substitute)
       (resolve-light-init (light-face-index schema))))

(defn resolve-light-sequence [schema intx]
  (->> intx
       (find-intersection (intersection-registration-index schema))
       (resolve-schedule)
       (find-light-template (light-group-index schema))
       (resolve-sequence)))

(defn resolve-all-rules [schema lane-id]
  (let [lane (find-lane schema lane-id)
        rule-set (resolve-rule-set-name lane)
        binders (match-binders schema rule-set)]
    (->> (resolve-intersection lane-id)
         (var->lane-index schema)
         (resolve-locals lane)
         (resolve-binders binders)
         (resolve-rules (rule-index schema)))))

