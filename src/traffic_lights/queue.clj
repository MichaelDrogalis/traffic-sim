(ns traffic-lights.queue
  (:require [clojure.core.reducers :as r]
            [clojure.pprint :refer [pprint]]))

(defn light-transition->fns [{:keys [state-diff ticks]}]
  (map (fn [_] (fn [light] (merge light state-diff))) (range ticks)))

(defn slot [lane id]
  (let [indexed-lane (zipmap lane (range))
        matches (filter (fn [[k v]] (= id (:id k))) indexed-lane)]
    (second (first matches))))

(defn back-of-car [car]
  (+ (:front car) (:len car)))

(defn drive-forward [car speed]
  (assert (>= (:front car) 0))
  (let [new-front (- (:front car) speed)]
    (assoc car :front (max new-front 0))))

(defn drive-watching-forward [car target speed]
  (let [space-between (- (:front car) (back-of-car target) (:buf car))]
    (assoc car :front (- (:front car) (min speed space-between)))))

(defn advance [speed old new-lane {:keys [id front buf] :as car}]
  (let [my-slot (slot old id)]
    (if (zero? my-slot)
      (conj new-lane (drive-forward car speed))
      (let [target (nth old (dec my-slot))]
        (conj new-lane (drive-watching-forward car target speed))))))

(defn next-lane-state [{:keys [state] :as lane}]
  (assoc lane :state (r/reduce (partial advance 1 state) [] state)))

(defn next-light-state [{:keys [state fns]}]
  (let [[f & more] fns]
    {:state (f state) :fns (conj (vec more) f)}))

(defn enqueue-into-ch [channel car]
  (assert (zero? (.size channel)))
  (.put channel car))

(defn add-to-lane [{:keys [state length] :as lane} {:keys [len] :as car}]
  (assoc lane :state (conj state (assoc car :front (- length len)))))

(defn take-from-channel [channel]
  (assert (<= (.size channel) 1))
  (.take channel))

(defn ch->lane [{:keys [channel state] :as lane}]
  (if-not (zero? (.size channel))
    (add-to-lane lane (take-from-channel channel))
    lane))

