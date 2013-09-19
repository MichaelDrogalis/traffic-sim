(ns traffic-lights.util)

(defn getx
  "Like two-argument get, but throws an exception if the key is not found."
  [m k]
  (let [e (get m k ::sentinel)]
    (if-not (= e ::sentinel)
      e
      (throw (ex-info "Missing required key" {:map m :key k})))))

(defn only
  "Like first, but throws unless exactly one item."
  [coll]
  (assert (not (next coll)))
  (if-let [result (first coll)]
    result
        (assert false)))

(defn maph [f coll & args]
  (apply merge (map (fn [[k v]] {k (apply f v args)}) coll)))

(defn without-ident [x]
  (dissoc x :street.lane.install/ident))

(defn lane-id [lane]
  (select-keys lane [:intersection/of :street/name :street/tag :street.lane.install/name]))

(defn index-by-lane-id [x]
  {(lane-id x) x})

(defn find-lane [target lanes]
  (first (filter (fn [x] (= (lane-id (:lane x)) target)) lanes)))

