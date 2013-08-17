(ns traffic-lights.core
  (:require [clojure.core.async :refer [chan go >! <! <!! timeout]]))

(defprotocol Touch
  (touch [this]))

(extend-protocol Touch
  clojure.lang.Ref
  (touch [x] (dosync (commute x identity)))
  clojure.lang.Agent
  (touch [x] (send x identity)))

(defprotocol GulpingQueue
  (offer! [this car])
  (gulp! [this car])
  (take! [this])
  (head [this]))

(defn back-of-car [car]
  (+ (:front car) (:length car)))

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
     (occupy-lane-space q car distance))))

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
                 (go (>! true))))))

(defn drive-forward [car]
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
              (<! ch)
              (remove-watch car preceeding-car))
            (drive-forward car)))
        (drive-forward car)))))

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

