(ns traffic-lights.core-test
  (:require [midje.sweet :refer :all]
            [traffic-lights.core :refer :all]))

(fact (legal-init-state? [:red :yellow :green] [:red]) => true)
(fact (legal-init-state? [:red :yellow :green] [:purple]) => false)
(fact (legal-init-state? [:red :yellow :green :left-green] [:green :left-green]) => true)

(fact (has-face? {:light/face :a}) => true)
(fact (has-face? {}) => false)

(fact (has-states? {:light/states [:red]}) => true)
(fact (has-states? {}) => false)

(fact (has-init? {:light/init [:red]}) => true)
(fact (has-init? {}) => false)

(fact (face-type-valid? :a) => true)
(fact (face-type-valid? []) => false)
(fact (face-type-valid? "a") => false)

(fact (states-type-valid? [:red]) => true)
(fact (states-type-valid? "red") => false)
(fact (states-type-valid? {}) => false)

(fact (init-type-valid? [:red]) => true)
(fact (init-type-valid? "red") => false)
(fact (init-type-valid? {}) => false)

(fact (non-empty-states? [:red]) => true)
(fact (non-empty-states? []) => false)

(fact (non-empty-init? [:red]) => true)
(fact (non-empty-init? []) => false)

(fact (only-keywords? [:red :yellow]) => true)
(fact (only-keywords? ["red"]) => false)
(fact (only-keywords? [:red 'yellow]) => false)

(fact (process-face {:light/face :a
                     :light/states [:red :yellow :green]
                     :light/init [:red]}))

