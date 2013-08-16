(ns traffic-lights.core
  (:require [clojure.core.async]))

(defprotocol GulpingQueue
  (offer! [this car f])
  (gulp! [this car])
  (take! [this])
  (head [this]))

(defn ref-gulping-offer! [q distance car f]
  (dosync
   (if-let [tail (last @q)]
     (let [room (- distance (+ (:front tail) (:length tail)))]
       (if (<= (:length car) room)
         (alter q conj (f car distance))
         tail))
     (alter q conj (f car distance)))))

(defn ref-gulping-take! [q]
  (dosync
   (let [head (first @q)]
     (alter q rest)
     head)))

(defn ref-gulping-head [q]
  (first @q))

(deftype RefQueue [line distance]
  GulpingQueue
  (offer! [this car f] (ref-gulping-offer! line distance car f))
  (take! [this] (ref-gulping-take! line))
  (head [this] (ref-gulping-head line))

  clojure.lang.IDeref
  (deref [this] @line))

(defn ref-gulping-queue [distance]
  (RefQueue. (ref []) distance))

(defn put-at-back! [car distance]
  (send car (fn [x] (assoc x :front (- distance (:length x))))))

(def queue (ref-gulping-queue 100))

(offer! queue (agent {:length 15}) put-at-back!)



