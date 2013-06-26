(ns traffic-lights.core)

(def light-schema {"North Street" {:states [:red :yellow :green] :init [:red]}
                   "South Street" {:states [:red :yellow :green] :init [:red]}
                   "East Street"  {:states [:red :yellow :green] :init [:red]}
                   "West Street"  {:states [:red :yellow :green] :init [:red]}})

(defn legal-init-state? [states init-vals]
  (every? (fn [x] (some #{x} states)) init-vals))

