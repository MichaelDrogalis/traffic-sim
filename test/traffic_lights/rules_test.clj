(ns traffic-lights.rules-test
  (:require [midje.sweet :refer :all]
            [traffic-lights.rules :refer :all]))

(fact (eval-atom '{:src ?a :dst ?b :yield []}
                 '{?a [{:id "a"}] ?b [{:id "b"}]})
      => '{:src {:id "a"} :dst {:id "b"} :yield []})

(fact (eval-atom '{:src ?a :dst ?b :yield [[?c]]}
                 '{?a [{:id "a"}] ?b [{:id "b"}] ?c [{:id "c"}]})
      => '{:src {:id "a"} :dst {:id "b"} :yield [[{:id "c"}]]})

(fact (eval-atom '{:src ?a :dst ?b :yield [[?c ?d]]}
                 '{?a [{:id "a"}] ?b [{:id "b"}] ?c [{:id "c"}] ?d [{:id "d"}]})
      => '{:src {:id "a"} :dst {:id "b"} :yield [[{:id "c"} {:id "d"}]]})

(fact (eval-atom '{:src ?a :dst ?b :yield []}
                 '{?b [{:id "b"}]})
      => (throws clojure.lang.ExceptionInfo))

(fact (eval-atom '{:src ?a :dst ?b :yield []}
                 '{?a [{:id "a"}]})
      => (throws clojure.lang.ExceptionInfo))

(fact (eval-atom '{:src ?a :dst ?b :yield [[?c]]}
                 '{?a [{:id "a"}] ?b [{:id "b"}]})
      => (throws clojure.lang.ExceptionInfo))

(fact (eval-atom '{:src ?a :dst ?b :yield [[?c ?d]]}
                 '{?a [{:id "a"}] ?b [{:id "b"}] ?c [{:id "c"}]})
      => (throws clojure.lang.ExceptionInfo))

