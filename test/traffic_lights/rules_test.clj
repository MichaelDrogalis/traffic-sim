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

(fact (local-var-index {:intersection/of ["a"]} {["a"] {"b" "c"}})
      => {"b" "c"})

(fact (local-var-index {:intersection/of ["b"]} {["a"] {}})
      => (throws clojure.lang.ExceptionInfo))

(fact (eval-local-lane-subs {:street.lane.install/substitute
                             {"x" "a" "y" "b" "z" "c"}}
                            {"a" 1 "b" 2 "c" 3})
      => {"x" 1 "y" 2 "z" 3})

(fact (eval-local-lane-subs {:street.lane.install/substitute
                             {"x" "a"}} {})
      => (throws clojure.lang.ExceptionInfo))

(fact (eval-binders [{:lane.rules/substitute {"x" "a" "y" "b" "z" "c"}}]
                    {"x" 1 "y" 2 "z" 3})
      {"x" 1 "y" 2 "z" 3})

(def atomic-index
  '{:straight
    {:rule/ident :straight
     :src ?origini
     :dst ?straighte
     :light [:green :yellow]}
    :left
    {:rule/ident :left
     :src ?origini
     :dst ?lefte
     :yield [[?straighti]]
     :light [:green :yellow]}
    :right
    {:rule/ident :right
     :src ?origini
     :dst ?righte
     :light [:green :yellow]}
    :right-on-red
    {:rule/ident :right-on-red
     :src ?origini
     :dst ?righte
     :yield [[?lefti ?righte]]
     :light [:red]}})

(def sub-index
  '{:shamrock
    [{:lane.rules/of :shamrock
      :lane.rules/register :straight
      :lane.rules/substitute {?origini ?origini ?straighte ?straighte}}
     {:lane.rules/of :shamrock
      :lane.rules/register :left
      :lane.rules/substitute
      {?origini ?origini
       ?lefte ?lefte
       ?straighti ?straighti
       ?origine ?origine}}
     {:lane.rules/of :shamrock
      :lane.rules/register :right
      :lane.rules/substitute {?origini ?origini ?righte ?righte}}
     {:lane.rules/of :shamrock
      :lane.rules/register :right-on-red
      :lane.rules/substitute
      {?origini ?origini ?righte ?righte ?lefti ?lefti}}]})

(def var-catalog
  '{["10th Street" "Market Street"]
    {?A
     [{:street.lane.install/ident ?A
       :street.lane.install/name "out"
       :street/tag "north"
       :street/name "10th Street"
       :intersection/of ["10th Street" "Market Street"]}]
     ?b
     [{:street.lane.install/ident ?b
       :street.lane.install/name "in"
       :street/tag "south"
       :street/name "10th Street"
       :intersection/of ["10th Street" "Market Street"]}]}})

(def lane
  '{:intersection/of ["10th Street" "Market Street"]
    :street/name "10th Street"
    :street/tag "south"
    :street.lane.install/name "in"
    :street.lane.install/rules :shamrock
    :street.lane.install/substitute {?origini ?b ?straighte ?A}})

(let [rules (eval-all-atomic-rules lane sub-index atomic-index var-catalog)
      expected '{:straight {:src [{:street.lane.install/ident ?b
                                   :street.lane.install/name "in"
                                   :street/tag "south"
                                   :street/name "10th Street"
                                   :intersection/of ["10th Street" "Market Street"]}]
                            :dst [{:street.lane.install/ident ?A
                                   :street.lane.install/name "out"
                                   :street/tag "north"
                                   :street/name "10th Street"
                                   :intersection/of ["10th Street" "Market Street"]}]
                            :yield []}
                 :left {:src [{:street.lane.install/ident ?b,
                               :street.lane.install/name "in",
                               :street/tag "south",
                               :street/name "10th Street",
                               :intersection/of ["10th Street" "Market Street"]}]
                        :dst ?lefte
                        :yield [[?straighti]]}
                 :right {:src [{:street.lane.install/ident ?b,
                                :street.lane.install/name "in",
                                :street/tag "south",
                                :street/name "10th Street",
                                :intersection/of ["10th Street" "Market Street"]}]
                         :dst ?righte
                         :yield []}
                 :right-on-red {:src [{:street.lane.install/ident ?b,
                                       :street.lane.install/name "in",
                                       :street/tag "south",
                                       :street/name "10th Street",
                                       :intersection/of ["10th Street" "Market Street"]}]
                                :dst ?righte
                                :yield [[?lefti ?righte]]}}]
  (doseq [r rules]
    (fact (:src (expected (:rule/ident r))) => (:src r))
    (fact (:dst (expected (:rule/ident r))) => (:dst r))
    (fact (:yield (expected (:rule/ident r))) => (:yield r))))

