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

(def queues-index (fmap (fn [_] (ref [])) street-catalog))

(def intx-catalog (build-non-unique-catalog schema :intersection/of))

(def intx-index (build-catalog schema :intersection/ident))

(def intx-area-index (fmap (fn [_] (ref [])) intx-index))

(defn street-lane-id-index [intx]
  (group-by :street.lane.install/ident
            (map #(select-keys %
                               [:street.lane.install/ident :intersection/of
                                :street/name :street/tag :street.lane.install/name])
                 (intx-catalog intx))))

(defn build-light-for-schedule [schedule light-catalog]
  (fmap #(:light-face/init (light-catalog %)) (:schedule/substitute schedule)))

(def traffic-light-index
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
    (loop []
        (doseq [{:keys [states duration] :as step} (:schedule/sequence schedule)]
          (send light (fn [x] (merge x states)))
          (Thread/sleep duration)))
    (recur)))

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

(defn semantic-var->4t [vars unevaled-street-map]
  (fmap #(vars %) unevaled-street-map))

(defn evaluate-rule-set [semantic-map rule-set]
  (apply merge (map (fn [sem-var] {sem-var (semantic-map sem-var)})
                    (:lane.rules/vars rule-set))))

(defn registered-rules [rule-sub-catalog street-catalog src]
  (rule-sub-catalog (:street.lane.install/rules (street-catalog src))))

(defn evaluate-rules [evaled-rule-binders]
  (map
   (fn [binder]
     (let [rule (rule-catalog (:lane.rules/register binder))
           sub-map (:lane.rules/substitute binder)]
       (-> rule
           (assoc :src (first (sub-map (:src rule))))
           (assoc :dst (first (sub-map (:dst rule))))
           (assoc :yield (map (fn [[src dst]] [(first (sub-map src)) (first (sub-map dst))]) (:yield rule))))))
   evaled-rule-binders))

(defn evaluate-rule-binders [binders semantic-map]
  (map (fn [binder]
         (assoc binder :lane.rules/substitute
                (fmap (fn [sem-var] (semantic-map sem-var)) (:lane.rules/substitute binder))))
       binders))

(defn applicable-rules [evaled-rules src dst light]
  (filter
   (fn [rule]
     (and (= (dissoc (:src rule) :street.lane.install/ident) src)
          (= (dissoc (:dst rule) :street.lane.install/ident) dst)
          (subset? light (into #{} (:light rule)))))
   evaled-rules))

(defn lane-is-empty? [src]
  (empty? @(queues-index src)))

(defn yield-lanes-clear? [rules]
  (map
   (fn [rule]
     (let [{:keys [dst yield]} rule]
       (every?
        true?
        (map
         (fn [[in & out]]
           (let [in-4t (dissoc in :street.lane.install/ident)]
             (if out
               (let [head-car (first (deref (queues-index in-4t)))]
                 (not= (:dst head-car) dst))
               (lane-is-empty? in-4t))))
         yield))))
   rules))

(defn safe-to-drive-through? [src dst light]
  (let [vars (street-lane-id-index (:intersection/of src))
        street-mapping (:street.lane.install/substitute (street-catalog src))
        semantic-map (semantic-var->4t vars street-mapping)
        binders (registered-rules rule-substitution-catalog street-catalog src)
        evaled-binders (evaluate-rule-binders binders semantic-map)
        evaled-rules (evaluate-rules evaled-binders)
        relevant-rules (applicable-rules evaled-rules src dst light)]
    (and (not (empty? relevant-rules))
         (yield-lanes-clear? relevant-rules))))

(defn drive-through-intersection [me]
  (alter (intx-area-index (:intersection/of (:src @me))) conj me)
  (Thread/sleep 2000)
  (alter (intx-area-index (:intersection/of (:src @me))) (partial filter (partial = me))))

(defn complicated-bit [])

(defn attempt-to-drive-through [me]
  (let [{:keys [src dst]} @me
        light-ident (:street.lane.install/light (street-catalog src))
        light ((deref (traffic-light-index (:intersection/of src))) light-ident)]
    (if (safe-to-drive-through? src dst light)
      (drive-through-intersection me)
      (complicated-bit))))

(defn last-driver-in-lane [src]
  (last @(queues-index src)))

(defn watch-car-ahead-of-me [target me]
  (add-watch target me
             (fn [_ _ _ car]
               (when-not (= (:src @car) (:src me))
                 (do (remove-watch car me)
                     (attempt-to-drive-through))))))

(defn put-me-in-queue [me]
  (let [queue (queues-index (:src @me))]
    (alter queue conj me)))

(defn drive-to-ingress-lane [me]
  (future
    (dosync
     (if (lane-is-empty? (:src @me))
       (do (put-me-in-queue me)
           (attempt-to-drive-through me))
       (let [tail (last-driver-in-lane (:src @me))]
         (watch-car-ahead-of-me tail me))))))

(defn turn-on-all-traffic-lights! []
  (doseq [[intx light] traffic-light-index]
    (add-watch light :printer (fn [_ _ _ state] (info state)))
    (turn-on-light! light (schedule-catalog (:intersection.install/schedule (intx-index intx))))))

(defn -main [& args]
  (turn-on-all-traffic-lights!)
  (Thread/sleep 100)
  (drive-to-ingress-lane mike))

