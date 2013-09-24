(ns traffic-lights.consumers.logger
  (:require [clojure.pprint :refer [pprint]]
            [traffic-lights.util :refer [maph]]))

(defn log! [idx]
  (pprint (maph :state idx)))

(defn log-snapshot [snapshot]
  (log! (:lights snapshot))
  (log! (:ingress snapshot))
  (log! (:egress snapshot)))

(defn watch-queue [queue]
  (add-watch
   queue :logger
   (fn [_ _ _ snapshot] (log-snapshot snapshot))))

