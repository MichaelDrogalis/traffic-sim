(ns traffic-lights.consumers.logger
  (:require [clojure.pprint :refer [pprint]]
            [traffic-lights.core :refer [queue]]
            [traffic-lights.util :refer [maph]]))

(defn log! [idx]
  (pprint (maph :state idx)))

(defn log-snapshot [snapshot]
  (log! (:lights snapshot))
  (log! (:ingress snapshot))
  (log! (:egress snapshot)))

(add-watch
 queue :logger
 (fn [_ _ _ snapshot] (log-snapshot snapshot)))

