(ns traffic-lights.core
  (:require [clojure.pprint :refer [pprint]]))

(def intersection-schema
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(defn catalog [schema kw]
  (filter (fn [x] (contains? x kw)) schema))

(defn build-light-catalog [schema]
  (catalog schema :light/ident))

(defn build-schedule-catalog [schema]
  (catalog schema :schedule/ident))

(defn build-light-set-catalog [schema]
  (catalog schema :light-set/ident))

(defn find-light [light-catalog ident]
  (first (filter (fn [x] (= (:light/ident x) ident)) light-catalog)))

(defn find-schedule [schedule-catalog ident]
  (first (filter (fn [x] (= (:schedule/ident x) ident)) schedule-catalog)))

(defn substitute-lights-in-light-set [light-catalog light-set]
  (apply merge
         (map (fn [[face light-ident]]
                {face (find-light light-catalog light-ident)})
              (:light-set/substitute light-set))))

(defn substitute-schedule-in-light-set [schedule-catalog light-set]
  (find-schedule schedule-catalog (:light-set/schedule light-set)))

(defn substitute-lights-in-schedule [schedule substitutions]
  (map (fn [{:keys [states] :as step}]
         (assoc step :states
                (merge (map (fn [[variable lights]]
                              {(substitutions variable) lights})
                            states))))
       schedule))

(defn construct-light [light-catalog schedule-catalog light-set]
  (let [lights (substitute-lights-in-light-set light-catalog light-set)
        schedule (substitute-schedule-in-light-set schedule-catalog light-set)
        schedule (substitute-lights-in-schedule (:light-set/schedule schedule) lights)]
    (assoc light-set :light-set/substitute lights :light-set/schedule schedule)))

(let [lc (build-light-catalog intersection-schema)
      sc (build-schedule-catalog intersection-schema)
      ls (build-light-set-catalog intersection-schema)]
  (doseq [x ls]
    (pprint (construct-light lc sc x))))

