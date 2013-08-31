(ns traffic-lights.index
  (:require [clojure.algo.generic.functor :refer [fmap]]))

(def schema
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(def connections
  (read-string (slurp (clojure.java.io/resource "connections-schema.edn"))))

(def drivers
  (read-string (slurp (clojure.java.io/resource "drivers.edn"))))

(def lane-identifiers [:street.lane.install/ident :intersection/of
                       :street/name :street/tag :street.lane.install/name])

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

(def light-face-index (build-catalog schema :light-face/ident))

(def light-group-schedule-index (build-catalog schema :schedule/ident))

(def atomic-rule-index (build-catalog schema :rule/ident))

(def intx-registration-index (build-catalog schema :intersection/ident))

(def intx-index (build-non-unique-catalog schema :intersection/of))

(def lane-rules-index (build-catalog schema :lane.rules/ident))

(def lanes-rules-subtitution-index (build-non-unique-catalog schema :lane.rules/of))

(def lane-index
  (ensure-uniqueness (build-composite-key-catalog
                      schema :intersection/of
                      lane-identifiers)))

(defn lane-catalog [intx street-name tag lane-name]
  (get lane-index
       {:intersection/of intx
        :street/name street-name
        :street/tag tag
        :street.lane.install/name lane-name}))

(def lane-state-index
  (fmap (fn [x] {:state [] :length (:street.lane.install/length x)}) lane-index))

(defn lane-var-catalog [intx]
  (group-by :street.lane.install/ident
            (map #(select-keys % lane-identifiers)
                 (intx-index intx))))

(defn build-light-for-schedule [schedule light-catalog]
  (fmap #(:light-face/init (light-face-index %)) (:schedule/substitute schedule)))

(def traffic-light-index
  (apply merge
         (map (fn [[intx _]]
                (let [light (-> (intx-registration-index intx)
                                :intersection.install/schedule
                                light-group-schedule-index
                                (build-light-for-schedule light-face-index))]
                  {intx light}))
              intx-index)))


