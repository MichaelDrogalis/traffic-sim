(ns traffic-lights.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.tools.logging :refer [info]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.queue :as q]
            [traffic-lights.index :as i]))

(defn drive [old-lanes old-lights]
  (let [new-lanes (apply merge (pmap q/next-lane-state old-lanes))
        new-lights (apply merge (pmap q/next-light-state old-lights))]
    (Thread/sleep 200)
    (recur new-lanes new-lights)))

