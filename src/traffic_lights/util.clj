(ns traffic-lights.util)

(defn getx
  "Like two-argument get, but throws an exception if the key is not found."
  [m k]
  (let [e (get m k ::sentinel)]
    (if-not (= e ::sentinel)
      e
      (throw (ex-info "Missing required key" {:map m :key k})))))

(defn maph [f coll & args]
  (apply merge (map (fn [[k v]] {k (apply f v args)}) coll)))

(defn without-ident [x]
  (dissoc x :street.lane.install/ident))

(defn local-var-index [lane var-catalog]
  (var-catalog (:intersection/of lane)))

(defn rule-set-name [lane]
  (:street.lane.install/rules lane))

(defn find-rule-from-binder [rule-index binder]
  (rule-index (:lane.rules/register binder)))

