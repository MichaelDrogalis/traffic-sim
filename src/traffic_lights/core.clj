(ns traffic-lights.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.tools.logging :refer [info]]
            [clojure.pprint :refer [pprint]]))

(def intersection-schema
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(defn ensure-uniqueness [catalog]
  (fmap first catalog))

(defn build-catalog [schema kw]
  (ensure-uniqueness (group-by kw (filter #(contains? % kw) schema))))

(defn build-light-catalog [schema]
  (build-catalog schema :light-face/ident))

(defn build-schedule-catalog [schema]
  (build-catalog schema :schedule/ident))

(defn build-rule-catalog [schema]
  (build-catalog schema :rule/ident))

(defn build-lane-rules-catalog [schema]
  (build-catalog schema :lane.rules/ident))

(defn build-rule-substitution-catalog [schema]
  (build-catalog schema :lane.rules/of))

(def light-catalog (build-light-catalog intersection-schema))

(def schedule-catalog (build-schedule-catalog intersection-schema))

(def rule-catalog (build-rule-catalog intersection-schema))

(def lane-rules (build-lane-rules-catalog intersection-schema))

(def rule-substitution-catalog (build-rule-substitution-catalog intersection-schema))

(defn build-light-for-schedule [schedule light-catalog]
  (fmap #(:light-face/init (light-catalog %)) (:schedule/substitute schedule)))

(defn turn-light-on [traffic-light schedule light-catalog]
  (doseq [step schedule]
    (let [{:keys [states duration]} step]
      (send traffic-light (fn [light] (apply merge light states)))
      (Thread/sleep duration))))

(defn turn-on-light [light schedule]
  (doseq [{:keys [states duration] :as step} schedule]
    (send light (fn [x] (merge x states)))
    (Thread/sleep duration)))

;;;;
(def schedule (schedule-catalog :shamrock-schedule))

(def traffic-light (agent (build-light-for-schedule schedule light-catalog)))

(def queues {{:intersection ["10th Street" "Chestnut Street"]
              :street "10th Street"
              :tag "south"
              :lane "in"}
             (agent [])})
;;;

(defn light-okay? [light src])

(defn no-one-to-yield-to? [src dst])

(defn drive-through-intersection [])


(defn attempt-to-drive-through [{:keys [src dst] :as me}]
  (let [light traffic-light]
    (if (and (light-okay? light src) (no-one-to-yield-to? src dst))
      (drive-through-intersection))))

(defn lane-is-empty? [src]
  (empty? (@queues src)))

(defn last-driver-in-lane [src]
  (last (@queues src)))

(defn watch-car-ahead-of-me [target me]
  (add-watch target me
             (fn [_ _ _ car]
               (when-not (= (:src @car) (:src me))
                 (do
                   (remove-watch car me)
                   (attempt-to-drive-through))))))

(defn drive-to-ingress-lane [{:keys [src dst] :as me}]
  (future
    (dosync
     (if (lane-is-empty? src)
       (attempt-to-drive-through me)
       (let [tail (last-driver-in-lane src)]
         (watch-car-ahead-of-me tail me))))))

;;; Begin experimentation.

(def mike (agent {:src {:intersection ["10th Street" "Chestnut Street"]
                        :street "10th Street"
                        :tag "south"
                        :lane "in"}
                  :dst {:intersection ["10th Street" "Chestnut Street"]
                        :street "10th Street"
                        :tag "north"
                        :lane "out"}
                  :id  (java.util.UUID/randomUUID)}))

(add-watch traffic-light :printer
           (fn [_ _ _ light]
             (info light)))

(defn -main [& args]
  (turn-on-light traffic-light (:schedule/sequence schedule)))

