(ns traffic-lights.scenarios.scenario-02-test
  "Driving one car up south street, halting for a red light & no other traffic."
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
     :schedule/sequence [{:state-diff {?x [:green]} :ticks 15}
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

(def south-in
  {:intersection/of ["Maple Street"]
   :street/name "Maple Street"
   :street/tag "south"
   :street.lane.install/name "in"})

(def north-out
  {:intersection/of ["Maple Street"]
   :street/name "Maple Street"
   :street/tag "north"
   :street.lane.install/name "out"})

(q/put-into-ch (:channel (get ingress-lanes south-in)) {:id "Mike" :len 1 :buf 0})

(def iterations
  (reduce (fn [world _] (conj world (t-fn (last world)))) [initial-world] (range 19)))

(def ingress-iterations
  (map (comp (partial u/find-lane south-in) vals) (map :ingress iterations)))

(def egress-iterations
  (map (comp (partial u/find-lane north-out) vals) (map :egress iterations)))

(def light-iterations
  (map (comp :state first vals) (map :lights iterations)))

(fact (:state (nth ingress-iterations 1))
      => [{:id "Mike" :len 1 :buf 0 :front 9 :dst north-out}])

(fact ('?y (nth light-iterations 10)) => [:red])

(fact (:state (nth ingress-iterations 10))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :dst north-out :ripe? false}])

(fact ('?y (nth light-iterations 11)) => [:red])

(fact (:state (nth ingress-iterations 11))
 => [{:id "Mike" :len 1 :buf 0 :front 0 :dst north-out :ripe? true}])

(fact ('?y (nth light-iterations 17)) => [:red])

(fact (:state (nth ingress-iterations 17))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :dst north-out :ripe? true}])

(fact ('?y (nth light-iterations 18)) => [:green])

(fact (:state (nth ingress-iterations 18))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :dst north-out :ripe? true}])

(fact ('?y (nth light-iterations 19)) => [:yellow])

(fact (:state (nth ingress-iterations 19))
      => [])

(fact (:state (nth egress-iterations 19))
      => [{:id "Mike" :len 1 :buf 0 :dst north-out :front 9}])

