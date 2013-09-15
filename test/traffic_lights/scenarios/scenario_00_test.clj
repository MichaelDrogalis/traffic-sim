(ns traffic-lights.scenarios.scenario-00-test
  "Sanity scenario of iterating a few times with no traffic."
  (:require [midje.sweet :refer :all]
            [traffic-lights.boot :as b]
            [traffic-lights.protocols :as p]
            [traffic-lights.rules :as r]
            [traffic-lights.queue :as q]
            [traffic-lights.util :as u]
            [traffic-lights.succession :refer :all]))

(def schema
  '[{:light-face/ident :standard
     :light-face/halt [:red]
     :light-face/proceed [:yellow :green]
     :light-face/init [:red]}
    
    {:schedule/ident :intx-schedule
     :schedule/substitute {?x :standard ?y :standard}
     :schedule/sequence [{:state-diff {?x [:green]} :ticks 1}
                         {:state-diff {?x [:yellow]} :ticks 1}
                         {:state-diff {?x [:red]} :ticks 1}
                         {:state-diff {?y [:green]} :ticks 1}
                         {:state-diff {?y [:yellow]} :ticks 1}
                         {:state-diff {?y [:red]} :ticks 1}]}
    
    {:rule/ident :straight
     :src ?origini
     :dst ?straighte
     :light [:green :yellow]}
    
    {:lane.rules/ident :no-turns
     :lane.rules/vars [?origini ?origine ?straighti ?straighte]}
    
    {:lane.rules/of :no-turns
     :lane.rules/register :straight
     :lane.rules/substitute {?origini ?origini ?straighte ?straighte}}

    {:intersection/ident ["Maple Street"]
     :intersection.install/schedule :intx-schedule}

    {:intersection/of ["Maple Street"]
     :street/name "Maple Street"
     :street/tag "north"
     :street.lane.install/name "in"
     :street.lane.install/ident ?a
     :street.lane.install/rules :no-turns
     :street.lane.install/type :ingress
     :street.lane.install/length 10
     :street.lane.install/light ?x
     :street.lane.install/substitute {?origini ?a
                                      ?origine ?A
                                      ?straighti ?b
                                      ?straighte ?B}}
    
    {:intersection/of ["Maple Street"]
     :street/name "Maple Street"
     :street/tag "south"
     :street.lane.install/name "in"
     :street.lane.install/ident ?b
     :street.lane.install/rules :no-turns
     :street.lane.install/type :ingress
     :street.lane.install/length 10
     :street.lane.install/light ?y
     :street.lane.install/substitute {?origini ?b
                                      ?origine ?B
                                      ?straighti ?a
                                      ?straighte ?A}}

    {:intersection/of ["Maple Street"]
     :street/name "Maple Street"
     :street/tag "north"
     :street.lane.install/name "out"
     :street.lane.install/ident ?A
     :street.lane.install/type :egress
     :street.lane.install/length 10}
    
    {:intersection/of ["Maple Street"]
     :street/name "Maple Street"
     :street/tag "south"
     :street.lane.install/name "out"
     :street.lane.install/ident ?B
     :street.lane.install/type :egress
     :street.lane.install/length 10}])

(def storage (p/memory-storage schema))

(def safety-fn (partial r/safe-to-go? storage))

(def dir-fn
  (constantly
   {:intersection/of ["Maple Street"]
    :street/name "Maple Street"
    :street/tag "north"
    :street.lane.install/name "out"}))

(def t-fn (transform-world-fn dir-fn safety-fn))

(def lights (into {} (map (partial b/boot-light storage) (p/intersections storage))))

(def ingress-lanes 
  (->> storage
       (p/ingress-lanes)
       (map u/index-by-lane-id)
       (into {})
       (u/maph b/boot-lane)))

(def egress-lanes
  (->> storage
       (p/egress-lanes)
       (map u/index-by-lane-id)
       (into {})
       (u/maph b/boot-lane)))

(def initial-world {:lights lights :ingress ingress-lanes :egress egress-lanes})

(def iterations
  (reduce (fn [world _] (conj world (t-fn (last world)))) [initial-world] (range 6)))

(def ingress-iterations
  (map (comp first vals) (map :ingress iterations)))

(def egress-iterations
  (map (comp first vals) (map :egress iterations)))

(def light-iterations
  (map (comp :state first vals) (map :lights iterations)))

(fact (every? #(empty? (:state %)) ingress-iterations) => true)
(fact (every? #(empty? (:channel %)) ingress-iterations) => true)

(fact (every? #(empty? (:state %)) egress-iterations) => true)
(fact (every? #(empty? (:channel %)) egress-iterations) => true)

(fact (nth light-iterations 0) => '{?x [:red] ?y [:red]})
(fact (nth light-iterations 1) => '{?x [:green] ?y [:red]})
(fact (nth light-iterations 2) => '{?x [:yellow] ?y [:red]})
(fact (nth light-iterations 3) => '{?x [:red] ?y [:red]})
(fact (nth light-iterations 4) => '{?x [:red] ?y [:green]})
(fact (nth light-iterations 5) => '{?x [:red] ?y [:yellow]})
(fact (nth light-iterations 6) => '{?x [:red] ?y [:red]})

