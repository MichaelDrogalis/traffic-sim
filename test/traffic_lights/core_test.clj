(ns traffic-lights.core-test
  (:require [midje.sweet :refer :all]
            [traffic-lights.core :refer :all]))

(fact (legal-init-state? [:red :yellow :green] [:red]) => true)
