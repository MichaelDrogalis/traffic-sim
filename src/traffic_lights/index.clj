(ns traffic-lights.index
  (:require [clojure.algo.generic.functor :refer [fmap]]))

(def schema
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(def connections
  (read-string (slurp (clojure.java.io/resource "connections-schema.edn"))))

(def drivers
  (read-string (slurp (clojure.java.io/resource "drivers.edn"))))

(def directions
  (read-string (slurp (clojure.java.io/resource "directions.edn"))))

(def lane-identifiers [:intersection/of :street/name :street/tag :street.lane.install/name])

(defn ensure-uniqueness [catalog]
  (fmap first catalog))

(defn group-by-key [schema kw]
  (group-by kw (filter #(contains? % kw) schema)))

(defn build-index [schema kw]
  (ensure-uniqueness (group-by-key schema kw)))

(defn build-non-unique-index [schema kw]
  (group-by-key schema kw))

(defn build-composite-key-index [schema id-kw kws]
  (group-by #(select-keys % kws) (filter #(contains? % id-kw) schema)))

(def drivers-index (build-index drivers :id))

(def directions-index (build-index directions :id))

(def light-face-index (build-index schema :light-face/ident))

(def light-group-schedule-index (build-index schema :schedule/ident))

(def atomic-rule-index (build-index schema :rule/ident))

(def intx-registration-index (build-index schema :intersection/ident))

(def intx-index (build-non-unique-index schema :intersection/of))

(def lane-rules-index (build-index schema :lane.rules/ident))

(def lanes-rules-subtitution-index (build-non-unique-index schema :lane.rules/of))

(def lane-index
  (ensure-uniqueness (build-composite-key-index
                      schema :intersection/of
                      lane-identifiers)))

(defn lane-catalog [intx street-name tag lane-name]
  (get lane-index
       {:intersection/of intx
        :street/name street-name
        :street/tag tag
        :street.lane.install/name lane-name}))

(def lane-state-index
  (fmap (fn [x] {:state []
                 :length (:street.lane.install/length x)
                 :channel (java.util.concurrent.LinkedBlockingQueue. 1)})
        lane-index))

(defn lane-var-catalog [intx]
  (let [index-keys [:intersection/of :street/name :street/tag
                    :street.lane.install/name :street.lane.install/ident]]
    (group-by :street.lane.install/ident
              (map #(select-keys % index-keys)
                   (intx-index intx)))))

(defn build-light-for-schedule [schedule light-catalog]
  (fmap #(:light-face/init (light-face-index %)) (:schedule/substitute schedule)))

(defn build-light-sequence [schedule]
  (:schedule/sequence (light-group-schedule-index schedule)))

(defn initial-light-state [intx]
  {:state-diff (-> (intx-registration-index intx)
                   :intersection.install/schedule
                   light-group-schedule-index
                   (build-light-for-schedule light-face-index))
   :ticks 0})

(def traffic-light-index
  (apply merge
         (map (fn [x]
                {x (cons
                    (initial-light-state x)
                    (build-light-sequence (:intersection.install/schedule
                                           (intx-registration-index x))))})
              (keys intx-index))))

