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
