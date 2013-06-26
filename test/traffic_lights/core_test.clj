(ns traffic-lights.core-test
  (:require [midje.sweet :refer :all]
            [traffic-lights.core :refer :all]))

(fact (legal-init-state? [:red :yellow :green] [:red]) => true)
(fact (legal-init-state? [:red :yellow :green] [:purple]) => false)
(fact (legal-init-state? [:red :yellow :green :left-green] [:green :left-green]) => true)

(fact (construct-light-face "North Street" [:red :yellow :green] [:red])
      => {"North Street" {:red true :yellow false :green false}})

