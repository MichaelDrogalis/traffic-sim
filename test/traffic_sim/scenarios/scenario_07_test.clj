(ns traffic-sim.scenarios.scenario-07-test
  "Driving one car up east on 11th and Walnut and straight to Broad."
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [midje.sweet :refer :all]
            [traffic-sim.boot :as b]
            [traffic-sim.protocols :as p]
            [traffic-sim.rules :as r]
            [traffic-sim.queue :as q]
            [traffic-sim.util :as u]
            [traffic-sim.succession :refer :all]))

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

(def walnut-12-east-in
  {:intersection/of ["12th Street" "Walnut Street"]
   :street/name "Walnut Street"
   :street/tag "east"
   :lane/name "in-3"})

(def walnut-12-west-out
  {:intersection/of ["12th Street" "Walnut Street"]
   :street/name "Walnut Street"
   :street/tag "west"
   :lane/name "out-2"})

(defn dir-fn [_ src]
  (let [lane-id (u/quad src)]
    (cond (= lane-id walnut-11-east-in) walnut-11-west-out
          (= lane-id walnut-11-west-out) walnut-12-east-in
          (= lane-id walnut-12-east-in) walnut-12-west-out)))

(def t-fn (transform-world-fn dir-fn dir-fn safety-fn))

(def lights (into {} (map (partial b/boot-light storage) (p/intersections storage))))

(def ingress-lanes (b/ingress-lanes storage))

(def egress-lanes (b/egress-lanes storage))

(def initial-world {:lights lights :ingress ingress-lanes :egress egress-lanes})

(q/put-into-ch (:channel (get ingress-lanes walnut-11-east-in)) {:id "Mike" :len 1 :buf 0})

(def iterations
  (reduce (fn [world _] (conj world (t-fn (last world)))) [initial-world] (range 30)))

(def ingress-11-east-iterations
  (map (comp (partial u/find-lane walnut-11-east-in) vals) (map :ingress iterations)))

(def egress-11-west-iterations
  (map (comp (partial u/find-lane walnut-11-west-out) vals) (map :egress iterations)))

(def ingress-12-east-iterations
  (map (comp (partial u/find-lane walnut-12-east-in) vals) (map :ingress iterations)))

(def egress-12-west-iterations
  (map (comp (partial u/find-lane walnut-12-west-out) vals) (map :egress iterations)))

(def light-iterations
  (map (fn [x] (fmap :state x)) (map :lights iterations)))

(fact (:state (nth ingress-11-east-iterations 1))
      => [{:id "Mike" :len 1 :buf 0 :front 49 :dst walnut-11-west-out}])

(fact (:state (nth ingress-11-east-iterations 10))
      => [{:id "Mike" :len 1 :buf 0 :front 4 :ripe? false :dst walnut-11-west-out}])

(fact (:state (nth ingress-11-east-iterations 11))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :ripe? false :dst walnut-11-west-out}])

(fact ('?x ((nth light-iterations 11) (:intersection/of walnut-11-east-in)))
      => [:green])

(fact (:state (nth ingress-11-east-iterations 12))
      => [])

(fact (:state (nth egress-11-west-iterations 13))
      => [{:id "Mike" :len 1 :buf 0 :front 57 :ripe? false :dst walnut-12-east-in}])

(fact (:state (nth egress-11-west-iterations 23))
      => [{:id "Mike" :len 1 :buf 0 :front 7 :ripe? false :dst walnut-12-east-in}])

(fact (:state (nth egress-11-west-iterations 24))
      => [{:id "Mike" :len 1 :buf 0 :front 2 :ripe? false :dst walnut-12-east-in}])

(fact (:state (nth egress-11-west-iterations 25))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :ripe? false :dst walnut-12-east-in}])

(fact (:state (nth egress-11-west-iterations 26))
      => [])

(fact (:state (nth ingress-12-east-iterations 26))
      => [{:id "Mike" :len 1 :buf 0 :front 59 :dst walnut-12-west-out}])

