(ns traffic-lights.protocols
  (:require [traffic-lights.resolve :as r]))

(defprotocol ILight
  (initial-light [this intx])
  (light-sequence [this intx]))

(defprotocol IResolve
  (resolve-rules [this lane-id]))

(deftype MemoryStorage [schema]
  ILight
  (initial-light [this intx] (r/resolve-initial-light schema intx))
  (light-sequence [this intx] (r/resolve-light-sequence schema intx))
  
  IResolve
  (resolve-rules [this lane-id] (r/resolve-all-rules schema lane-id)))

(defn memory-storage [schema]
  (MemoryStorage. schema))

