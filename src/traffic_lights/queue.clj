(ns traffic-lights.core
  (:require [clojure.core.async]))

(defprotocol GulpingQueue
  (offer! [this car])
  (gulp! [this car])
  (take! [this])
  (head [this]))

(defn put-at-back! [car distance]
  (send car (fn [x] (assoc x :front (- distance (:length x))))))

(defn occupy-lane-space [q car distance]
  (alter q conj (put-at-back! car distance))
  nil)

(defn ref-offer! [q distance car]
  (dosync
   (if-let [tail (last @q)]
     (let [room (- distance (+ (:front @tail) (:length @tail)))]
       (if (<= (:length @car) room)
         (occupy-lane-space q car distance)
         tail))
     (occupy-lane-space q car distance)))
  (await))

(defn drive-distance [car]
  (let [step 5
        space-left (:front car)]
    (if (> space-left step)
      step
      space-left)))

(defn ref-gulp! [q car]
  (while (> (:front @car) 0)
    (send car (fn [x] (assoc x :front (- (:front x) (drive-distance x)))))
    (await car)))

(defn ref-take! [q]
  (dosync
   (let [head (first @q)]
     (alter q rest)
     head)))

(defn ref-head [q]
  (first @q))

(deftype RefQueue [line distance]
  GulpingQueue
  (offer! [this car] (ref-offer! line distance car))
  (gulp! [this car] (ref-gulp! line car))
  (take! [this] (ref-take! line))
  (head [this] (ref-head line))

  clojure.lang.IDeref
  (deref [this] @line))

(defn ref-gulping-queue [distance]
  (RefQueue. (ref []) distance))

(def queue (ref-gulping-queue 100))

