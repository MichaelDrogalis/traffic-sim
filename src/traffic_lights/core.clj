(ns traffic-lights.core
  (:require [clojure.pprint :refer [pprint]]
            [traffic-lights.index :as i]
            [traffic-lights.transform :as t]
            [traffic-lights.boot :as b]
            [traffic-lights.util :refer [maph]]))

(def indexed-boot-light
  (partial b/boot-light
           i/intx-registration-index
           i/light-group-schedule-index
           i/light-face-index))

(def lights
  (apply merge (map indexed-boot-light (keys i/intx-registration-index))))

(defn log-lights! [idx]
  (pprint (maph :state idx)))

(defn genesis! [old-lights]
  (log-lights! old-lights)
  (let [new-lights (t/transform-lights old-lights)]
    (recur new-lights)))

