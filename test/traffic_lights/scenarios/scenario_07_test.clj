(ns traffic-lights.scenarios.scenario-07-test
  "Driving one car up east 11th and down Walnut heading west."
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [midje.sweet :refer :all]
            [traffic-lights.boot :as b]
            [traffic-lights.protocols :as p]
            [traffic-lights.rules :as r]
            [traffic-lights.queue :as q]
            [traffic-lights.util :as u]
            [traffic-lights.succession :refer :all]))

(def intersections
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(def connections
  (read-string (slurp (clojure.java.io/resource "connections-schema.edn"))))

(def storage (p/memory-storage (concat intersections connections)))

(def safety-fn (r/safe-to-go? storage))

(def walnut-11-east-in
  {:intersection/of ["11th Street" "Walnut Street"]
   :street/name "Walnut Street"
   :street/tag "east"
   :lane/name "in-2"})

(def walnut-11-west-out
  {:intersection/of ["11th Street" "Walnut Street"]
   :street/name "Walnut Street"
   :street/tag "west"
   :lane/name "out-2"})

(def dir-fn
  (constantly walnut-11-west-out))

(def t-fn (transform-world-fn dir-fn safety-fn))

(def lights (into {} (map (partial b/boot-light storage) (p/intersections storage))))

(def ingress-lanes (b/ingress-lanes storage))

(def egress-lanes (b/egress-lanes storage))

(def initial-world {:lights lights :ingress ingress-lanes :egress egress-lanes})

(q/put-into-ch (:channel (get ingress-lanes walnut-11-east-in)) {:id "Mike" :len 1 :buf 0})

(def iterations
  (reduce (fn [world _] (conj world (t-fn (last world)))) [initial-world] (range 100)))

(def ingress-east-iterations
  (map (comp (partial u/find-lane walnut-11-east-in) vals) (map :ingress iterations)))

(def egress-west-iterations
  (map (comp (partial u/find-lane walnut-11-west-out) vals) (map :egress iterations)))

(def light-iterations
  (map (fn [x] (fmap :state x)) (map :lights iterations)))

(fact (:state (nth ingress-east-iterations 1))
      => [{:id "Mike" :len 1 :buf 0 :front 49 :dst walnut-11-west-out}])

(fact (:state (nth ingress-east-iterations 45))
      => [{:id "Mike" :len 1 :buf 0 :front 5 :ripe? false :dst walnut-11-west-out}])

(fact (:state (nth ingress-east-iterations 49))
      => [{:id "Mike" :len 1 :buf 0 :front 1 :ripe? false :dst walnut-11-west-out}])

(fact ('?x ((nth light-iterations 50) (:intersection/of walnut-11-east-in)))
      => [:red])

(fact (:state (nth ingress-east-iterations 50))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :ripe? false :dst walnut-11-west-out}])

(fact (:state (nth ingress-east-iterations 51))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :ripe? true :dst walnut-11-west-out}])

(fact ('?x ((nth light-iterations 60) (:intersection/of walnut-11-east-in)))
      => [:red])

(fact (:state (nth ingress-east-iterations 60))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :ripe? true :dst walnut-11-west-out}])

(fact ('?x ((nth light-iterations 61) (:intersection/of walnut-11-east-in)))
      => [:green])

(fact (:state (nth ingress-east-iterations 61))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :ripe? true :dst walnut-11-west-out}])

(fact (:state (nth ingress-east-iterations 62))
      => [])

(fact (:state (nth egress-west-iterations 62))
      => [{:id "Mike" :len 1 :buf 0 :front 72 :dst walnut-11-west-out}])

