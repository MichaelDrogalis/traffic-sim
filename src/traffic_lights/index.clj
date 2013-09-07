(ns traffic-lights.index
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.pprint :refer [pprint]])
  (:import [java.util.concurrent LinkedBlockingQueue]))

(def schema
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(def connections
  (read-string (slurp (clojure.java.io/resource "connections-schema.edn"))))

(def drivers
  (read-string (slurp (clojure.java.io/resource "drivers.edn"))))

(def directions
  (read-string (slurp (clojure.java.io/resource "directions.edn"))))

(def lane-identifiers [:intersection/of :street/name :street/tag :street.lane.install/name])

(defn build-catalog [schema kw]
  (filter #(contains? % kw) schema))

(defn ensure-uniqueness [catalog]
  (fmap first catalog))

(defn group-by-key [schema kw]
  (group-by kw (filter #(contains? % kw) schema)))

(defn build-non-unique-index [schema kw]
  (group-by-key schema kw))

(defn build-index [schema kw]
  (ensure-uniqueness (build-non-unique-index schema kw)))

(defn build-composite-key-index [schema id-kw kws]
  (group-by #(select-keys % kws) (filter #(contains? % id-kw) schema)))

(def drivers-index (build-index drivers :id))

(def directions-index (build-index directions :id))

(def light-face-index (build-index schema :light-face/ident))

(def light-group-schedule-index (build-index schema :schedule/ident))

(def intx-registration-index (build-index schema :intersection/ident))

(def intx-index (build-non-unique-index schema :intersection/of))

(def atomic-rule-index (build-index schema :rule/ident))

(def lane-rules-index (build-index schema :lane.rules/ident))

(def lanes-rules-substitution-index (build-non-unique-index schema :lane.rules/of))

(def lane-index
  (ensure-uniqueness (build-composite-key-index
                      schema :intersection/of
                      lane-identifiers)))

(def lane-catalogv (build-catalog schema :intersection/of))

(defn lane-catalog [intx street-name tag lane-name]
  (get lane-index
       {:intersection/of intx
        :street/name street-name
        :street/tag tag
        :street.lane.install/name lane-name}))

(defn initialize-lane [lanes]
  (map (fn [x] {:lane x :state [] :channel (LinkedBlockingQueue. 1)}) lanes))

(def ingress-lane-catalog (filter #(= (:street.lane.install/type %) :ingress) lane-catalogv))

(def egress-lane-catalog (filter #(= (:street.lane.install/type %) :egress) lane-catalogv))

(def ingress-lane-state-catalog (initialize-lane ingress-lane-catalog))

(def egress-lane-state-catalog (initialize-lane egress-lane-catalog))

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

(def traffic-light-catalog
  (map (fn [x]
         (let [intx-reg (intx-registration-index x)
               initial (initial-light-state x)
               subsequent (build-light-sequence (:intersection.install/schedule intx-reg))]
           {:intersection x :state-seq (cons initial subsequent)}))
       (keys intx-index)))

