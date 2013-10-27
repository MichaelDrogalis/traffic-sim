(ns traffic-sim.protocols
  (:require [traffic-sim.memory :as m]))

(defprotocol ILight
  (initial-light [this intx])
  (light-sequence [this intx]))

(defprotocol IIntersection
  (intersections [this]))

(defprotocol ILane
  (lanes [this])
  (ingress-lanes [this])
  (egress-lanes [this])
  (find-lane [this quad]))

(defprotocol IResolve
  (resolve-rules [this lane-id]))

(defprotocol IInternalLink
  (internal-links [this quad]))

(defprotocol IExternalLink
  (external-links [this quad])
  (external-reverse-links [this quad]))

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
  (find-lane [this quad] (m/find-lane schema quad))
  
  IResolve
  (resolve-rules [this lane-id] (m/resolve-all-rules schema lane-id))

  IInternalLink
  (internal-links [this quad] (m/match-internal-links schema quad))
  
  IExternalLink
  (external-links [this quad] (m/match-external-links schema quad))
  (external-reverse-links [this quad] (m/match-external-reverse-links schema quad)))

(defn memory-storage [schema]
  (MemoryStorage. schema))

