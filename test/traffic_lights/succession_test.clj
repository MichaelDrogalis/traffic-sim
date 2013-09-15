(ns traffic-lights.succession-test
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

    {:intersection/ident ["Maple Street"],
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

(def six-iterations
  (reduce (fn [world _] (conj world (t-fn (last world)))) [initial-world] (range 6)))

(def six-ingress-iterations
  (map (comp first vals) (map :ingress six-iterations)))

(def six-egress-iterations
  (map (comp first vals) (map :egress six-iterations)))

(def six-light-iterations
  (map (comp :state first vals) (map :lights six-iterations)))

(fact (every? #(empty? (:state %)) six-ingress-iterations) => true)
(fact (every? #(empty? (:channel %)) six-ingress-iterations) => true)

(fact (every? #(empty? (:state %)) six-egress-iterations) => true)
(fact (every? #(empty? (:channel %)) six-egress-iterations) => true)

(fact (nth six-light-iterations 0) => '{?x [:red] ?y [:red]})
(fact (nth six-light-iterations 1) => '{?x [:green] ?y [:red]})
(fact (nth six-light-iterations 2) => '{?x [:yellow] ?y [:red]})
(fact (nth six-light-iterations 3) => '{?x [:red] ?y [:red]})
(fact (nth six-light-iterations 4) => '{?x [:red] ?y [:green]})
(fact (nth six-light-iterations 5) => '{?x [:red] ?y [:yellow]})
(fact (nth six-light-iterations 6) => '{?x [:red] ?y [:red]})

(def south-in
  {:intersection/of ["Maple Street"]
   :street/name "Maple Street"
   :street/tag "south"
   :street.lane.install/name "in"})

(q/put-into-ch (:channel (get ingress-lanes south-in)) {:id "Mike" :len 1 :buf 0})

(def twenty-iterations
  (reduce (fn [world _] (conj world (t-fn (last world)))) [initial-world] (range 21)))

(def twenty-ingress-iterations
  (map (comp first vals) (map :ingress twenty-iterations)))

(def twenty-egress-iterations
  (map (comp second vals) (map :egress twenty-iterations)))

(def twenty-light-iterations
  (map (comp :state first vals) (map :lights twenty-iterations)))

(fact (:state (nth twenty-ingress-iterations 1))
      => [{:id "Mike" :len 1 :buf 0 :front 9}])

(fact (:state (nth twenty-ingress-iterations 2))
      => [{:id "Mike" :len 1 :buf 0 :front 8 :ripe? false}])

(fact (:state (nth twenty-ingress-iterations 3))
      => [{:id "Mike" :len 1 :buf 0 :front 7 :ripe? false}])

(fact (:state (nth twenty-ingress-iterations 4))
      => [{:id "Mike" :len 1 :buf 0 :front 6 :ripe? false}])

(fact (:state (nth twenty-ingress-iterations 5))
      => [{:id "Mike" :len 1 :buf 0 :front 5 :ripe? false}])

(fact (:state (nth twenty-ingress-iterations 6))
      => [{:id "Mike" :len 1 :buf 0 :front 4 :ripe? false}])

(fact (:state (nth twenty-ingress-iterations 7))
      => [{:id "Mike" :len 1 :buf 0 :front 3 :ripe? false}])

(fact (:state (nth twenty-ingress-iterations 8))
      => [{:id "Mike" :len 1 :buf 0 :front 2 :ripe? false}])

(fact (:state (nth twenty-ingress-iterations 9))
      => [{:id "Mike" :len 1 :buf 0 :front 1 :ripe? false}])

(fact (:state (nth twenty-ingress-iterations 10))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :ripe? false}])

(fact ('?y (nth twenty-light-iterations 10)) => [:green])

(fact (:state (nth twenty-ingress-iterations 11))
      => [])

(fact (:state (nth twenty-egress-iterations 11))
      => [{:id "Mike" :len 1 :buf 0 :front 9}])

(fact (:state (nth twenty-egress-iterations 12))
      => [{:id "Mike" :len 1 :buf 0 :front 8 :ripe? false}])

(fact (:state (nth twenty-egress-iterations 13))
      => [{:id "Mike" :len 1 :buf 0 :front 7 :ripe? false}])

(fact (:state (nth twenty-egress-iterations 14))
      => [{:id "Mike" :len 1 :buf 0 :front 6 :ripe? false}])

(fact (:state (nth twenty-egress-iterations 15))
      => [{:id "Mike" :len 1 :buf 0 :front 5 :ripe? false}])

(fact (:state (nth twenty-egress-iterations 16))
      => [{:id "Mike" :len 1 :buf 0 :front 4 :ripe? false}])

(fact (:state (nth twenty-egress-iterations 17))
      => [{:id "Mike" :len 1 :buf 0 :front 3 :ripe? false}])

(fact (:state (nth twenty-egress-iterations 18))
      => [{:id "Mike" :len 1 :buf 0 :front 2 :ripe? false}])

(fact (:state (nth twenty-egress-iterations 19))
      => [{:id "Mike" :len 1 :buf 0 :front 1 :ripe? false}])

(fact (:state (nth twenty-egress-iterations 20))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :ripe? false}])

(fact (:state (nth twenty-egress-iterations 21))
      => [])

