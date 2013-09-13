(ns traffic-lights.protocols
  (:require [traffic-lights.resolve :as r]))

(defprotocol ILight
  (initial-light [this intx])
  (light-sequence [this intx]))

(defprotocol IIntersection
  (intersections [this]))

(defprotocol ILane
  (lanes [this])
  (ingress-lanes [this])
  (egress-lanes [this]))

(defprotocol IResolve
  (resolve-rules [this lane-id]))

(deftype MemoryStorage [schema]
  ILight
  (initial-light [this intx] (r/resolve-initial-light schema intx))
  (light-sequence [this intx] (r/resolve-light-sequence schema intx))

  IIntersection
  (intersections [this] (r/intersection-index schema))
  
  ILane
  (lanes [this] (r/lane-index schema))
  (ingress-lanes [this] (r/ingress-lane-index schema))
  (egress-lanes [this] (r/egress-lane-index schema))
  
  IResolve
  (resolve-rules [this lane-id] (r/resolve-all-rules schema lane-id)))

(defn memory-storage [schema]
  (MemoryStorage. schema))

