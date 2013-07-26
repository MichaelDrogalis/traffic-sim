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

(def light-catalog (build-light-catalog intersection-schema))

(def schedule-catalog (build-schedule-catalog intersection-schema))

(defn build-light-for-schedule [schedule light-catalog]
  (fmap #(:light-face/init (light-catalog %)) (:schedule/substitute schedule)))

(defn turn-light-on [traffic-light schedule light-catalog]
  (doseq [step schedule]
    (let [{:keys [states duration]} step]
      (send traffic-light (fn [light] (apply merge light states)))
      (Thread/sleep duration))))

;;; Begin experimentation.

(def schedule (schedule-catalog :shamrock-schedule))

(def traffic-light
  (agent (build-light-for-schedule schedule light-catalog)))

(add-watch traffic-light :printer
           (fn [_ _ _ light]
             (info light)))

(defn turn-on-light [light schedule]
  (doseq [{:keys [states duration] :as step} schedule]
    (send light (fn [x] (merge x states)))
    (Thread/sleep duration)))

(defn -main [& args]
  (turn-on-light traffic-light (:schedule/sequence schedule)))

