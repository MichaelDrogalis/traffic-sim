(ns traffic-lights.consumers.reload
  (:require [traffic-lights.core :refer [queue] :as c]
            [traffic-lights.util :refer [maph]]
            [traffic-lights.queue :as q]))

(def last-count (atom 0))

(def spawn-lane
  {:intersection/of ["Juniper Street" "Walnut Street"]
   :street/name "Juniper Street"
   :street/tag "north"
   :lane/name "in"})

(defn spawn-driver! []
  (q/put-into-ch (:channel (c/ingress-lanes spawn-lane))
                 {:id (str (java.util.UUID/randomUUID)) :buf 5 :len 1}))

(defn reload-drivers [snapshot]
  (let [count-drivers #(apply + (map count (vals (maph :state (get snapshot %)))))
        ingress-drivers (count-drivers :ingress)
        egress-drivers (count-drivers :egress)
        total-drivers (+ ingress-drivers egress-drivers)]
    (when (< total-drivers @last-count)
      (spawn-driver!))
    (swap! last-count (constantly total-drivers))))

(add-watch
 queue :reloader
 (fn [_ _ _ snapshot] (reload-drivers snapshot)))

