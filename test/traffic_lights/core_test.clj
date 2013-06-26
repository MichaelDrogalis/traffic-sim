(ns traffic-lights.core-test
  (:require [midje.sweet :refer :all]
            [traffic-lights.core :refer :all]))

(fact (legal-init-state? [:red :yellow :green] [:red]) => true)
(fact (legal-init-state? [:red :yellow :green] [:purple]) => false)
(fact (legal-init-state? [:red :yellow :green :left-green] [:green :left-green]) => true)

(fact (construct-light-face "North Street" [:red :yellow :green] [:red])
      => {"North Street" {:red true :yellow false :green false}})

(fact (construct-light-face "North Street" [:red :yellow :green] [:blue])
      => (throws clojure.lang.ExceptionInfo))

(fact (construct-light {"North Street" {:states [:red :yellow :green] :init [:red]}
                        "South Street" {:states [:red :yellow :green] :init [:red]}
                        "East Street"  {:states [:red :yellow :green] :init [:red]}
                        "West Street"  {:states [:red :yellow :green] :init [:red]}})
      => {"North Street" {:red true :yellow false :green false}
          "South Street" {:red true :yellow false :green false}
          "East Street"  {:red true :yellow false :green false}
          "West Street"  {:red true :yellow false :green false}})

