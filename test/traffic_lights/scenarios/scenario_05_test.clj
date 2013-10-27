(ns traffic-lights.scenarios.scenario-05-test
  "Driving one car up south street taking a left, yielding to traffic."
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
     :schedule/sequence [{:state-diff {?x [:red] ?y [:red]} :ticks 6}
                         {:state-diff {?x [:green] ?y [:green]} :ticks 5}]}
    
    {:rule/ident :straight
     :src ?origini
     :dst ?straighte
     :light [:green :yellow]}

    {:rule/ident :left
     :src ?origini
     :dst ?lefte
     :yield [[?straighti]]
     :light [:green :yellow]}
    
    {:lane.rules/ident :no-turns
     :lane.rules/vars [?origini ?straighte]}

    {:lane.rules/ident :left-only
     :lane.rules/vars [?origini ?straighti ?lefte]}
    
    {:lane.rules/of :no-turns
     :lane.rules/register :straight
     :lane.rules/substitute {?origini ?origini ?straighte ?straighte}}

    {:lane.rules/of :left-only
     :lane.rules/register :left
     :lane.rules/substitute {?origini ?origini ?straighti ?straighti ?lefte ?lefte}}

    {:intersection/ident ["Maple Street" "Leaf Street"]
     :intersection.install/schedule :intx-schedule}

    {:intersection/of ["Maple Street" "Leaf Street"]
     :street/name "Maple Street"
     :street/tag "north"
     :lane/name "in"
     :street.lane.install/ident ?a
     :street.lane.install/rules :no-turns
     :street.lane.install/type :ingress
     :street.lane.install/length 3
     :street.lane.install/speed-limit 1
     :street.lane.install/light ?x
     :street.lane.install/substitute {?origini ?a
                                      ?straighte ?B}}
    
    {:intersection/of ["Maple Street" "Leaf Street"]
     :street/name "Maple Street"
     :street/tag "south"
     :lane/name "in"
     :street.lane.install/ident ?b
     :street.lane.install/rules :left-only
     :street.lane.install/type :ingress
     :street.lane.install/length 3
     :street.lane.install/speed-limit 1
     :street.lane.install/light ?y
     :street.lane.install/substitute {?origini ?b
                                      ?straighti ?a
                                      ?lefte ?C}}

    {:intersection/of ["Maple Street" "Leaf Street"]
     :street/name "Maple Street"
     :street/tag "north"
     :lane/name "out"
     :street.lane.install/ident ?A
     :street.lane.install/type :egress
     :street.lane.install/length 3
     :street.lane.install/speed-limit 1}
    
    {:intersection/of ["Maple Street" "Leaf Street"]
     :street/name "Maple Street"
     :street/tag "south"
     :lane/name "out"
     :street.lane.install/ident ?B
     :street.lane.install/type :egress
     :street.lane.install/length 3
     :street.lane.install/speed-limit 1}

    {:intersection/of ["Maple Street" "Leaf Street"]
     :street/name "Leaf Street"
     :street/tag "west"
     :lane/name "out"
     :street.lane.install/ident ?C
     :street.lane.install/type :egress
     :street.lane.install/length 3
     :street.lane.install/speed-limit 1}])

(def storage (p/memory-storage schema))

(def safety-fn (r/safe-to-go? storage))

(defn dir-fn [id _]
  (if (= id "Mike")
    {:intersection/of ["Maple Street" "Leaf Street"]
     :street/name "Leaf Street"
     :street/tag "west"
     :lane/name "out"}
    {:intersection/of ["Maple Street" "Leaf Street"]
     :street/name "Maple Street"
     :street/tag "south"
     :lane/name "out"}))

(def t-fn (transform-world-fn dir-fn dir-fn safety-fn))

(def lights (into {} (map (partial b/boot-light storage) (p/intersections storage))))

(def ingress-lanes (b/ingress-lanes storage))

(def egress-lanes (b/egress-lanes storage))

(def initial-world {:lights lights :ingress ingress-lanes :egress egress-lanes})

(def north-in
  {:intersection/of ["Maple Street" "Leaf Street"]
   :street/name "Maple Street"
   :street/tag "north"
   :lane/name "in"})

(def south-in
  {:intersection/of ["Maple Street" "Leaf Street"]
   :street/name "Maple Street"
   :street/tag "south"
   :lane/name "in"})

(def south-out
  {:intersection/of ["Maple Street" "Leaf Street"]
   :street/name "Maple Street"
   :street/tag "south"
   :lane/name "out"})

(def west-out
  {:intersection/of ["Maple Street" "Leaf Street"]
   :street/name "Leaf Street"
   :street/tag "west"
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

(def egress-west-iterations
  (map (comp (partial u/find-lane west-out) vals) (map :egress iterations)))

(def light-iterations
  (map (comp :state first vals) (map :lights iterations)))

(fact (:state (nth ingress-south-iterations 1))
      => [{:id "Mike" :len 1 :buf 0 :front 2 :dst west-out}])

(fact (:state (nth ingress-south-iterations 2))
      => [{:id "Mike" :len 1 :buf 0 :front 1 :dst west-out :ripe? false}])

(fact (:state (nth ingress-south-iterations 3))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :dst west-out :ripe? false}])

(fact (:state (nth ingress-south-iterations 4))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :dst west-out :ripe? true}])

(fact (:state (nth ingress-south-iterations 5))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :dst west-out :ripe? true}])

(fact ('?y (nth light-iterations 6)) => [:red])

(fact (:state (nth ingress-south-iterations 6))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :dst west-out :ripe? true}])

(fact (:state (nth ingress-north-iterations 6))
      => [{:id "Kristen" :len 1 :buf 0 :front 0 :dst south-out :ripe? true}])

(fact ('?y (nth light-iterations 7)) => [:green])

(fact (:state (nth ingress-south-iterations 7))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :dst west-out :ripe? true}])

(fact (:state (nth ingress-north-iterations 7))
      => [{:id "Kristen" :len 1 :buf 0 :front 0 :dst south-out :ripe? true}])

(fact (:state (nth ingress-north-iterations 8))
      => [])

(fact (:state (nth egress-south-iterations 8))
      => [{:id "Kristen" :len 1 :buf 0 :dst south-out :front 2}])

(fact (:state (nth ingress-south-iterations 8))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :dst west-out :ripe? true}])

(fact (:state (nth ingress-south-iterations 9))
      => [])

(fact (:state (nth egress-west-iterations 9))
      => [{:id "Mike" :len 1 :buf 0 :dst west-out :front 2}])

