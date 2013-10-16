(ns traffic-lights.scenarios.scenario-08-test
  "Driving one car up east on 13th and Walnut and north up Broad."
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

(def walnut-13-east-in
  {:intersection/of ["13th Street" "Walnut Street"]
   :street/name "Walnut Street"
   :street/tag "east"
   :lane/name "in-3"})

(def walnut-13-north-out
  {:intersection/of ["13th Street" "Walnut Street"]
   :lane/name "out-2"
   :street/tag "north"
   :street/name "13th Street"})

(defn dir-fn [_ src]
  (let [lane-id (u/quad src)]
    (cond (= lane-id walnut-13-east-in) walnut-13-north-out
          :else nil)))

(def t-fn (transform-world-fn dir-fn dir-fn safety-fn))

(def lights (into {} (map (partial b/boot-light storage) (p/intersections storage))))

(def ingress-lanes (b/ingress-lanes storage))

(def egress-lanes (b/egress-lanes storage))

(def initial-world {:lights lights :ingress ingress-lanes :egress egress-lanes})

(q/put-into-ch (:channel (get ingress-lanes walnut-13-east-in)) {:id "Mike" :len 1 :buf 0})

(def iterations
  (reduce (fn [world _] (conj world (t-fn (last world)))) [initial-world] (range 200)))

(def ingress-13-east-iterations
  (map (comp (partial u/find-lane walnut-13-east-in) vals) (map :ingress iterations)))

(def egress-13-north-iterations
  (map (comp (partial u/find-lane walnut-13-north-out) vals) (map :egress iterations)))

(def light-iterations
  (map (fn [x] (fmap :state x)) (map :lights iterations)))

(fact (:state (nth ingress-13-east-iterations 1))
      => [{:id "Mike" :len 1 :buf 0 :front 34 :dst walnut-13-north-out}])

(fact (:state (nth ingress-13-east-iterations 2))
      => [{:id "Mike" :len 1 :buf 0 :front 24 :ripe? false :dst walnut-13-north-out}])

(fact (:state (nth ingress-13-east-iterations 3))
      => [{:id "Mike" :len 1 :buf 0 :front 14 :ripe? false :dst walnut-13-north-out}])

(fact (:state (nth ingress-13-east-iterations 4))
      => [{:id "Mike" :len 1 :buf 0 :front 4 :ripe? false :dst walnut-13-north-out}])

(fact (:state (nth ingress-13-east-iterations 5))
      => [{:id "Mike" :len 1 :buf 0 :front 0 :ripe? false :dst walnut-13-north-out}])

(fact ('?x ((nth light-iterations 5) (:intersection/of walnut-13-east-in)))
      => [:green])

(fact (:state (nth ingress-13-east-iterations 6))
      => [])

(fact (:state (nth egress-13-north-iterations 6))
      => [{:id "Mike" :len 1 :buf 0 :front 73 :dst nil}])

(fact (:state (nth egress-13-north-iterations 7))
      => [{:id "Mike" :len 1 :buf 0 :front 63 :dst nil :ripe? false}])

