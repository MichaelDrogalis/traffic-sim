(ns traffic-lights.core-test
  (:require [midje.sweet :refer :all]
            [traffic-lights.core :refer :all]))

(fact (parallel-map-merge identity {"Mike" 22}) => {"Mike" 22})
(fact (parallel-map-merge inc {"Mike" 22}) => {"Mike" 23})


