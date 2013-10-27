(ns traffic-sim.consumers.logger
  (:require [clojure.pprint :refer [pprint]]
            [traffic-sim.core :refer [queue]]
            [traffic-sim.util :refer [maph]]))

(defn log! [idx]
  (pprint (maph :state idx)))

(defn log-snapshot [snapshot]
  (log! (:ingress snapshot))
  (log! (:egress snapshot)))

(add-watch
 queue :logger
 (fn [_ _ _ snapshot] (log-snapshot snapshot)))

