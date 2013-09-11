(ns traffic-lights.protocols
  (:require [traffic-lights.resolve :as r]))

(defprotocol ILight
  (initial-light [this intx]))

(deftype MemoryStorage [schema]
  ILight
  (initial-light [this intx]
    (r/resolve-initial-light schema intx)))

(defn memory-storage [schema]
  (MemoryStorage. schema))

