(ns traffic-lights.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.tools.logging :refer [info]]
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

(def intx-catalog (build-non-unique-catalog schema :intersection/of))

(defn street-lane-id-index [intx]
  (group-by :street.lane.install/ident
            (map #(select-keys %
                               [:street.lane.install/ident :intersection/of
                                :street/name :street/tag :street.lane.install/name])
                 (intx-catalog intx))))

(defn build-light-for-schedule [schedule light-catalog]
  (fmap #(:light-face/init (light-catalog %)) (:schedule/substitute schedule)))

(defn turn-light-on [traffic-light schedule light-catalog]
  (doseq [step schedule]
    (let [{:keys [states duration]} step]
      (send traffic-light (fn [light] (apply merge light states)))
      (Thread/sleep duration))))

(defn turn-on-light [light schedule]
  (future
    (doseq [{:keys [states duration] :as step} schedule]
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

(def traffic-light (agent (build-light-for-schedule schedule light-catalog)))

(def queues {{:intersection/of ["10th Street" "Chestnut Street"]
              :street/name "10th Street"
              :street/tag "south"
              :street.lane.install/name "in"}
             (agent [])})

(def straight-rule (:straight rule-catalog))

(def src (:src @mike))
(def dst (:dst @mike))

(def vars (street-lane-id-index (:intersection/of src)))

(def street-mapping (:street.lane.install/substitute (street-catalog src)))

(def semantic-var->4t (fmap #(vars %) street-mapping))

;;;

(defn light-okay? [light src]
  (prn light)
  (prn src))

(defn no-one-to-yield-to? [src dst])

(defn drive-through-intersection [])

(defn complicated-bit [])

(defn attempt-to-drive-through [me]
  (let [light traffic-light
        {:keys [src dst]} @me]
    (if (and (light-okay? light src) (no-one-to-yield-to? src dst))
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

;;; Begin experimentation.

(add-watch traffic-light :printer
           (fn [_ _ _ light]
;             (info light)
             ))

(defn -main [& args]
  (turn-on-light traffic-light (:schedule/sequence schedule))
  (drive-to-ingress-lane mike))

