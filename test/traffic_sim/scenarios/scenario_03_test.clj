(ns traffic-sim.scenarios.scenario-03-test
  "Driving one car up south street and one car down north street.
   The light is constantly green, and symmetric to both streets."
  (:require [midje.sweet :refer :all]
            [traffic-sim.boot :as b]
            [traffic-sim.protocols :as p]
            [traffic-sim.rules :as r]
            [traffic-sim.queue :as q]
            [traffic-sim.util :as u]
            [traffic-sim.succession :refer :all]))

(def schema
  '[{:light-face/ident :standard
     :light-face/halt [:red]
     :light-face/proceed [:yellow :green]
     :light-face/init [:red]}
    
    {:schedule/ident :intx-schedule
     :schedule/substitute {?x :standard ?y :standard}
     :schedule/sequence [{:state-diff {?x [:green] ?y [:green]} :ticks 1}]}
    
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
     :lane/name "in"
     :street.lane.install/ident ?a
     :street.lane.install/rules :no-turns
     :street.lane.install/type :ingress
     :street.lane.install/length 10
     :street.lane.install/speed-limit 1
     :street.lane.install/light ?x
     :street.lane.install/substitute {?origini ?a
                                      ?origine ?A
                                      ?straighti ?b
                                      ?straighte ?B}}
    
    {:intersection/of ["Maple Street"]
     :street/name "Maple Street"
     :street/tag "south"
     :lane/name "in"
     :street.lane.install/ident ?b
     :street.lane.install/rules :no-turns
     :street.lane.install/type :ingress
     :street.lane.install/length 10
     :street.lane.install/speed-limit 1
     :street.lane.install/light ?y
     :street.lane.install/substitute {?origini ?b
                                      ?origine ?B
                                      ?straighti ?a
                                      ?straighte ?A}}

    {:intersection/of ["Maple Street"]
     :street/name "Maple Street"
     :street/tag "north"
     :lane/name "out"
     :street.lane.install/ident ?A
     :street.lane.install/type :egress
     :street.lane.install/length 10
     :street.lane.install/speed-limit 1}
    
    {:intersection/of ["Maple Street"]
     :street/name "Maple Street"
     :street/tag "south"
     :lane/name "out"
     :street.lane.install/ident ?B
     :street.lane.install/type :egress
     :street.lane.install/length 10
     :street.lane.install/speed-limit 1}])

(def storage (p/memory-storage schema))

(def safety-fn (r/safe-to-go? storage))

(defn dir-fn [id _]
  (if (= id "Mike")
    {:intersection/of ["Maple Street"]
     :street/name "Maple Street"
     :street/tag "north"
     :lane/name "out"}
    {:intersection/of ["Maple Street"]
     :street/name "Maple Street"
     :street/tag "south"
     :lane/name "out"}))

(def t-fn (transform-world-fn dir-fn dir-fn safety-fn))

(def lights (b/lights storage))

(def ingress-lanes (b/ingress-lanes storage))

(def egress-lanes (b/egress-lanes storage))

(def initial-world {:lights lights :ingress ingress-lanes :egress egress-lanes})

(def south-in
  {:intersection/of ["Maple Street"]
   :street/name "Maple Street"
   :street/tag "south"
   :lane/name "in"})

(def north-in
  {:intersection/of ["Maple Street"]
   :street/name "Maple Street"
   :street/tag "north"
   :lane/name "in"})

(def south-out
  {:intersection/of ["Maple Street"]
   :street/name "Maple Street"
   :street/tag "south"
   :lane/name "out"})

(def north-out
  {:intersection/of ["Maple Street"]
   :street/name "Maple Street"
   :street/tag "north"
   :lane/name "out"})

(q/put-into-ch (:channel (get ingress-lanes south-in)) {:id "Mike" :len 1 :buf 0})
(q/put-into-ch (:channel (get ingress-lanes north-in)) {:id "Kristen" :len 1 :buf 0})

(def iterations
  (reduce (fn [world _] (conj world (t-fn (last world)))) [initial-world] (range 21)))

(def ingress-south-iterations
  (map (comp (partial u/find-lane south-in) vals) (map :ingress iterations)))

(def ingress-north-iterations
  (map (comp (partial u/find-lane north-in) vals) (map :ingress iterations)))

(def egress-south-iterations
  (map (comp (partial u/find-lane south-out) vals) (map :egress iterations)))

(def egress-north-iterations
  (map (comp (partial u/find-lane north-out) vals) (map :egress iterations)))

(def light-iterations
  (map (comp :state first vals) (map :lights iterations)))

(fact (:state (nth ingress-south-iterations 1))
      => [{:id "Mike" :len 1 :buf 0 :dst north-out :front 9}])

(fact (:state (nth ingress-north-iterations 1))
      => [{:id "Kristen" :len 1 :buf 0 :dst south-out :front 9}])

(fact (:state (nth ingress-south-iterations 9))
      => [{:id "Mike" :len 1 :buf 0 :front 1 :dst north-out :ripe? false}])

(fact (:state (nth ingress-north-iterations 9))
      => [{:id "Kristen" :len 1 :buf 0 :front 1 :dst south-out :ripe? false}])

(fact (:state (nth ingress-south-iterations 10))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :dst north-out :ripe? false}])

(fact (:state (nth ingress-north-iterations 10))
      => [{:id "Kristen" :len 1 :buf 0 :front 0 :dst south-out :ripe? false}])

(fact (:state (nth ingress-south-iterations 11))
      => [])

(fact (:state (nth ingress-north-iterations 11))
      => [])

(fact (:state (nth egress-south-iterations 11))
      => [{:id "Kristen" :len 1 :buf 0 :dst south-out :front 9}])

(fact (:state (nth egress-north-iterations 11))
      => [{:id "Mike" :len 1 :buf 0 :front 9 :dst north-out}])

(fact (:state (nth egress-south-iterations 20))
      => [{:id "Kristen" :len 1 :buf 0 :front 0 :dst south-out :ripe? false}])

(fact (:state (nth egress-north-iterations 20))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :dst north-out :ripe? false}])

