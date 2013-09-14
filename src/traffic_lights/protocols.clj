(ns traffic-lights.protocols
  (:require [traffic-lights.memory :as m]))

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
  (initial-light [this intx] (m/resolve-initial-light schema intx))
  (light-sequence [this intx] (m/resolve-light-sequence schema intx))

  IIntersection
  (intersections [this] (m/intersection-index schema))
  
  ILane
  (lanes [this] (m/lane-index schema))
  (ingress-lanes [this] (m/ingress-lane-index schema))
  (egress-lanes [this] (m/egress-lane-index schema))
  
  IResolve
  (resolve-rules [this lane-id] (m/resolve-all-rules schema lane-id)))

(defn memory-storage [schema]
  (MemoryStorage. schema))

