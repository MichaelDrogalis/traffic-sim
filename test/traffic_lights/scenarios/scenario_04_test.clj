(ns traffic-lights.scenarios.scenario-04-test
  "Driving one car up south street taking a left, no traffic."
  (:require [midje.sweet :refer :all]
            [traffic-lights.boot :as b]
            [traffic-lights.protocols :as p]
            [traffic-lights.rules :as r]
            [traffic-lights.queue :as q]
            [traffic-lights.util :refer [lane-id] :as u]
            [traffic-lights.succession :refer :all]))

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

    {:rule/ident :left
     :src ?origini
     :dst ?lefte
     :yield [[?straighti]]
     :light [:green :yellow]}
    
    {:lane.rules/ident :no-turns
     :lane.rules/vars [?origini ?origine ?straighti ?straighte ?lefte]}
    
    {:lane.rules/of :no-turns
     :lane.rules/register :straight
     :lane.rules/substitute {?origini ?origini ?straighte ?straighte}}

    {:lane.rules/of :no-turns
     :lane.rules/register :left
     :lane.rules/substitute {?origini ?origini ?straighti ?straighti ?lefte ?lefte}}

    {:intersection/ident ["Maple Street" "Leaf Street"]
     :intersection.install/schedule :intx-schedule}

    {:intersection/of ["Maple Street" "Leaf Street"]
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
    
    {:intersection/of ["Maple Street" "Leaf Street"]
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
                                      ?straighte ?A
                                      ?lefte ?C}}

    {:intersection/of ["Maple Street" "Leaf Street"]
     :street/name "Maple Street"
     :street/tag "north"
     :street.lane.install/name "out"
     :street.lane.install/ident ?A
     :street.lane.install/type :egress
     :street.lane.install/length 10}
    
    {:intersection/of ["Maple Street" "Leaf Street"]
     :street/name "Maple Street"
     :street/tag "south"
     :street.lane.install/name "out"
     :street.lane.install/ident ?B
     :street.lane.install/type :egress
     :street.lane.install/length 10}

    {:intersection/of ["Maple Street" "Leaf Street"]
     :street/name "Leaf Street"
     :street/tag "west"
     :street.lane.install/name "out"
     :street.lane.install/ident ?C
     :street.lane.install/type :egress
     :street.lane.install/length 10}])

(def storage (p/memory-storage schema))

(def safety-fn (partial r/safe-to-go? storage))

(def dir-fn
  (constantly
   {:intersection/of ["Maple Street" "Leaf Street"]
    :street/name "Leaf Street"
    :street/tag "west"
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
  {:intersection/of ["Maple Street" "Leaf Street"]
   :street/name "Maple Street"
   :street/tag "south"
   :street.lane.install/name "in"})

(def west-out
  {:intersection/of ["Maple Street" "Leaf Street"]
   :street/name "Leaf Street"
   :street/tag "west"
   :street.lane.install/name "out"})

(q/put-into-ch (:channel (get ingress-lanes south-in)) {:id "Mike" :len 1 :buf 0})

(def iterations
  (reduce (fn [world _] (conj world (t-fn (last world)))) [initial-world] (range 21)))

(defn find-lane [target lanes]
  (first (filter (fn [x] (= (lane-id (:lane x)) target)) lanes)))

(def ingress-south-iterations
  (map (comp (partial find-lane south-in) vals) (map :ingress iterations)))

(def egress-west-iterations
  (map (comp (partial find-lane west-out) vals) (map :egress iterations)))

(fact (:state (nth ingress-south-iterations 1))
      => [{:id "Mike" :len 1 :buf 0 :front 9}])

(fact (:state (nth ingress-south-iterations 10))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :ripe? false}])

(fact (:state (nth ingress-south-iterations 11))
      => [])

(fact (:state (nth egress-west-iterations 11))
      => [{:id "Mike" :len 1 :buf 0 :front 9}])

