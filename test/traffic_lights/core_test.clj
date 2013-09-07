(ns traffic-lights.core-test
  (:require [midje.sweet :refer :all]
            [traffic-lights.core :refer :all]))

(fact (par-map-merge (fn [_ x] (identity x)) {"Mike" 22}) => {"Mike" 22})
(fact (par-map-merge (fn [_ x] (inc x)) {"Mike" 22}) => {"Mike" 23})


