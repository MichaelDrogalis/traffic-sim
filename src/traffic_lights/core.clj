(ns traffic-lights.core)

(def light-schema {"North Street" {:states [:red :yellow :green] :init [:red]}
                   "South Street" {:states [:red :yellow :green] :init [:red]}
                   "East Street"  {:states [:red :yellow :green] :init [:red]}
                   "West Street"  {:states [:red :yellow :green] :init [:red]}})

(defn legal-init-state? [states init-vals]
  (every? (fn [x] (some #{x} states)) init-vals))

(defn construct-light-face [street states init-vals]
  {street (merge (zipmap states (repeat false))
                 (zipmap init-vals (repeat true)))})
