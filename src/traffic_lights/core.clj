(ns traffic-lights.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.tools.logging :refer [info]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]))

(def schema
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(defn ensure-uniqueness [catalog]
  (fmap first catalog))

(defn group-by-key [schema kw]
  (group-by kw (filter #(contains? % kw) schema)))

(defn build-catalog [schema kw]
  (ensure-uniqueness (group-by-key schema kw)))

(defn build-non-unique-catalog [schema kw]
  (group-by-key schema kw))

(defn build-composite-key-catalog [schema id-kw kws]
  (group-by #(select-keys % kws) (filter #(contains? % id-kw) schema)))

(def light-catalog (build-catalog schema :light-face/ident))

(def schedule-catalog (build-catalog schema :schedule/ident))

(def rule-catalog (build-catalog schema :rule/ident))

(def lane-rules (build-catalog schema :lane.rules/ident))

(def rule-substitution-catalog (build-non-unique-catalog schema :lane.rules/of))

(def street-catalog
  (ensure-uniqueness (build-composite-key-catalog
                      schema :intersection/of
                      [:intersection/of
                       :street/name
                       :street/tag 
                       :street.lane.install/name])))

(def queues (fmap (fn [_] (agent [])) street-catalog))

(def intx-catalog (build-non-unique-catalog schema :intersection/of))

(def intx-index (build-catalog schema :intersection/ident))

(defn street-lane-id-index [intx]
  (group-by :street.lane.install/ident
            (map #(select-keys %
                               [:street.lane.install/ident :intersection/of
                                :street/name :street/tag :street.lane.install/name])
                 (intx-catalog intx))))

(defn build-light-for-schedule [schedule light-catalog]
  (fmap #(:light-face/init (light-catalog %)) (:schedule/substitute schedule)))

(defn traffic-light-index [intx-catalog light-catalog]
  (apply merge
         (map (fn [[intx _]]
                {intx (agent (-> intx
                                 intx-index
                                 :intersection.install/schedule
                                 schedule-catalog
                                 (build-light-for-schedule light-catalog)))})
              intx-catalog)))

(defn turn-on-light! [light schedule]
  (future
    (doseq [{:keys [states duration] :as step} (:schedule/sequence schedule)]
      (send light (fn [x] (merge x states)))
      (Thread/sleep duration))))

;;;;
(def mike (agent {:src {:intersection/of ["10th Street" "Chestnut Street"]
                        :street/name "10th Street"
                        :street/tag "south"
                        :street.lane.install/name "in"}
                  :dst {:intersection/of ["10th Street" "Chestnut Street"]
                        :street/name "10th Street"
                        :street/tag "north"
                        :street.lane.install/name "out"}
                  :id  (java.util.UUID/randomUUID)}))

(def schedule (schedule-catalog :shamrock-schedule))

(def src (:src @mike))

(def dst (:dst @mike))

(def light-to-watch (:street.lane.install/light (street-catalog src)))

(def vars (street-lane-id-index (:intersection/of src)))

(def street-mapping (:street.lane.install/substitute (street-catalog src)))

(def semantic-var->4t (fmap #(vars %) street-mapping))

(def rule-set (lane-rules (:street.lane.install/rules (street-catalog src))))

(defn evaluate-rule-set [rule-set]
  (apply merge (map (fn [sem-var] {sem-var (semantic-var->4t sem-var)})
                    (:lane.rules/vars rule-set))))

(defn registered-rules [rule-sub-catalog street-catalog src]
  (rule-sub-catalog (:street.lane.install/rules (street-catalog src))))

(defn evaluate-rule-binders [binders]
  (map (fn [binder]
         (assoc binder :lane.rules/substitute
                (fmap #(evaluate-rule-set %) (:lane.rules/substitute binder))))
       binders))

(defn evaluate-rules [evaled-rule-binders]
 (map
  (fn [binder]
    (let [rule (rule-catalog (:lane.rules/register binder))
          sub-map (:lane.rules/substitute binder)]
      (-> rule
          (assoc :src (sub-map (:src rule)))
          (assoc :dst (sub-map (:dst rule)))
          (assoc :yield (map (fn [[src dst]] [(sub-map src) (sub-map dst)]) (:yield rule))))))
  evaled-rule-binders))

(defn applicable-rules [evaled-rules src dst light]
 (filter
  (fn [rule]
    (and (= (dissoc (first (:src rule)) :street.lane.install/ident) src)
         (= (dissoc (first (:dst rule)) :street.lane.install/ident) dst)
         (subset? (light light-to-watch) (into #{} (:light rule)))))
  evaled-rules))

;;;

(defn safe-to-drive-through? [src dst light]
  (let [binders (registered-rules rule-substitution-catalog street-catalog src)
        evaled-rules (evaluate-rules binders)]
    (not (empty? (applicable-rules evaled-rules src dst light)))))

(defn drive-through-intersection [])

(defn complicated-bit [])

(defn attempt-to-drive-through [me light]
  (let [{:keys [src dst]} @me]
    (if (safe-to-drive-through? src dst ((traffic-light-index intx-catalog light-catalog) src))
      (drive-through-intersection)
      (complicated-bit))))

(defn lane-is-empty? [src]
  (empty? @(queues src)))

(defn last-driver-in-lane [src]
  (last @(queues src)))

(defn watch-car-ahead-of-me [target me]
  (add-watch target me
             (fn [_ _ _ car]
               (when-not (= (:src @car) (:src me))
                 (do
                   (remove-watch car me)
                   (attempt-to-drive-through))))))

(defn drive-to-ingress-lane [me]
  (future
    (dosync
     (if (lane-is-empty? (:src @me))
       (attempt-to-drive-through me)
       (let [tail (last-driver-in-lane (:src @me))]
         (watch-car-ahead-of-me tail me))))))

(defn -main [& args]
  (doseq [[intx light] (traffic-light-index intx-catalog light-catalog)]
    (add-watch light :printer (fn [_ _ _ state] (info state)))
    (turn-on-light! light (schedule-catalog (:intersection.install/schedule (intx-index intx))))))

