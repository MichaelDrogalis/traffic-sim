(ns traffic-lights.queue
  (:require [clojure.core.async :refer [chan go >! <! <!! timeout]]))

(defprotocol Touch
  (touch [this]))

(extend-protocol Touch
  clojure.lang.Ref
  (touch [x] (dosync (commute x identity)))
  clojure.lang.Agent
  (touch [x] (send x identity)))

(defprotocol SpatialEmpty
  (front-empty? [this])
  (back-empty? [this]))

(defprotocol Peekable
  (front-peek [this])
  (back-peek [this]))

(defprotocol GulpingQueue
  (offer! [this car])
  (gulp! [this car])
  (take! [this]))

(defn back-of-car [car]
  (+ (:front car) (:length car)))

(defn put-at-back! [car distance]
  (send car (fn [x] (assoc x :front (- distance (:length x))))))

(defn occupy-lane-space [q car distance]
  (alter q conj (put-at-back! car distance))
  nil)

(defn ref-offer! [q distance car]
  (let [result
        (dosync
         (if-let [tail (last @q)]
           (let [room (- distance (+ (:front @tail) (:length @tail)))]
             (if (<= (:length @car) room)
               (occupy-lane-space q car distance)
               tail))
           (occupy-lane-space q car distance)))]
    (await car)
    result))

(def car-step 5)

(def buffer-space 3)

(defn drive-distance [car]
  (let [step car-step
        space-left (:front car)]
    (if (> space-left step)
      step
      car-step)))

(defn drive-step [car]
  (assoc car :front (- (:front car) (drive-distance car))))

(defn watch-car-for-motion [me car ch]
  (add-watch car me
             (fn [_ _ old new]
               (when-not (= old new)
                 (go (>! ch true))))))

(defn drive-forward [car]
  (prn (:id @car) ": " (:front @car))
  (send car drive-step)
  (<!! (timeout 500))
  (await car))

(defn ref-gulp! [q car]
  (while (> (:front @car) 0)
    (let [preceeding-pos (dec (.indexOf @q car))]
      (if-not (neg? preceeding-pos)
        (let [preceeding-car (nth @q preceeding-pos)]
          (if (<= (- (:front @car) (back-of-car @preceeding-car)) buffer-space)
            (let [ch (chan)]
              (watch-car-for-motion car preceeding-car ch)
              (touch preceeding-car)
              (<!! ch)
              (remove-watch car preceeding-car))
            (drive-forward car)))
        (drive-forward car)))))

(defn ref-take! [q]
  (dosync
   (let [head (first @q)]
     (alter q rest)
     (send head dissoc :front)
     head)))

(defn ref-front-peek [q]
  (first @q))

(defn ref-back-peek [q]
  (last @q))

(defn ref-front-empty? [q]
  (or (empty? @q) (>= (:front (deref (first @q))) 25)))

(defn ref-back-empty? [q]
  (or (empty? @q) (<= (:front (deref (last @q))) 80)))

(deftype RefQueue [line distance]
  GulpingQueue
  (offer! [this car] (ref-offer! line distance car))
  (gulp! [this car] (ref-gulp! line car))
  (take! [this] (ref-take! line))

  Peekable
  (front-peek [this] (ref-front-peek line))
  (back-peek [this] (ref-back-peek line))
  
  SpatialEmpty
  (front-empty? [this] (ref-front-empty? line))
  (back-empty? [this] (ref-back-empty? line))

  clojure.lang.IRef
  (addWatch [this key cb] (add-watch line key cb))
  (removeWatch [this key] (remove-watch line key))
  
  clojure.lang.IDeref
  (deref [this] @line)

  Touch
  (touch [this] (touch line)))

(deftype NullQueue [line]
  GulpingQueue
  (offer! [this car])
  (gulp! [this car])
  (take! [this] nil)

  Peekable
  (back-peek [this] nil)
  
  SpatialEmpty
  (back-empty? [this] true)

  clojure.lang.IDeref
  (deref [this] @line)

  Touch
  (touch [this] (touch line)))

(defn ref-gulping-queue [distance]
  (RefQueue. (ref []) distance))

(defn null-gulping-queue []
  (NullQueue. (ref [])))

