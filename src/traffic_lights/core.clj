(ns traffic-lights.core
  (:require [dire.core :refer [with-precondition!]]))

(defn legal-init-state? [states init-vals]
  (every? (fn [x] (some #{x} states)) init-vals))

(defn construct-light-face [street states init-vals]
  {street (merge (zipmap states (repeat false))
                 (zipmap init-vals (repeat true)))})

(with-precondition! #'construct-light-face
  :valid-init-vals
  (fn [_ states init-vals]
    (legal-init-state? states init-vals)))

(defn construct-light [schema]
  (reduce (fn [result [street {:keys [states init]}]]
            (conj result (construct-light-face street states init)))
          {} schema))


(def light-schema {"North Street" {:states [:red :yellow :green] :init [:red]}
                   "South Street" {:states [:red :yellow :green] :init [:red]}
                   "East Street"  {:states [:red :yellow :green] :init [:red]}
                   "West Street"  {:states [:red :yellow :green] :init [:red]}})

(def light-schedule [{:states {"North Street" [:green] "South Street" [:green]} :duration 8000}
                     {:states {"North Street" [:yellow] "South Street" [:yellow]} :duration 1000}
                     {:states {"North Street" [:red] "South Street" [:red]} :duration 1000}
                     {:states {"East Street" [:green] "West Street" [:green]} :duration 8000}
                     {:states {"East Street" [:yellow] "West Street" [:yellow]} :duration 1000}
                     {:states {"East Street" [:red] "West Street" [:red]} :duration 1000}])

(def streets {"North Street" {"Main lane" {"South Street" {:light #{:green :yellow}}
                                           "West Street"  {:light #{:green :yellow}}
                                           "East Street"  {:light #{:green :yellow}
                                                           :opposing-traffic #{{"South Street" #{"North Street" "East Street"}}}}}}
              "South Street" {"Main lane" {:light #{:green :yellow}
                                           :opposing-traffic #{{"North Street" #{"East Street"}}}}}
              "East Street"  {"Main lane" {:light #{:green :yellow}
                                           :opposing-traffic #{{"West Street" #{"North Street"}}}}}
              "West Street"  {"Main lane" {:light #{:green :yellow}
                                           :opposing-traffic #{{"East Street" #{"South Street"}}}}}})

