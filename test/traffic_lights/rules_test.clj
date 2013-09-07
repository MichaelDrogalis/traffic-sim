(ns traffic-lights.rules-test
  (:require [midje.sweet :refer :all]
            [traffic-lights.rules :refer :all]))

(fact (eval-atom '{:src ?a :dst ?b :yield []}
                 '{?a [{:id "a"}] ?b [{:id "b"}]})
      => '{:src {:id "a"} :dst {:id "b"} :yield []})

(fact (eval-atom '{:src ?a :dst ?b :yield [[?c]]}
                 '{?a [{:id "a"}] ?b [{:id "b"}] ?c [{:id "c"}]})
      => '{:src {:id "a"} :dst {:id "b"} :yield [[{:id "c"} nil]]})

(fact (eval-atom '{:src ?a :dst ?b :yield [[?c ?d]]}
                 '{?a [{:id "a"}] ?b [{:id "b"}] ?c [{:id "c"}] ?d [{:id "d"}]})
      => '{:src {:id "a"} :dst {:id "b"} :yield [[{:id "c"} {:id "d"}]]})


