(ns traffic-lights.queue
  (:require [clojure.core.reducers :as r]
            [traffic-lights.util :refer [quad]]))

(defn light-transition->fns [{:keys [state-diff ticks]}]
  (map (fn [_] (fn [light] (merge light state-diff))) (range ticks)))

(defn slot [lane id]
  (let [indexed-lane (zipmap lane (range))
        matches (filter (fn [[k v]] (= id (:id k))) indexed-lane)]
    (second (first matches))))

(defn back-of-car [car]
  (+ (:front car) (:len car)))

(defn drive-forward [car speed]
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

(defn next-light-state [{:keys [state fns]}]
  (let [[f & more] fns]
    {:state (f state) :fns (conj (vec more) f)}))

(defn put-into-ch [channel car]
  (.put channel car))

(defn take-from-channel [channel]
  (.take channel))

(defn add-to-lane [{:keys [lane state] :as entity} {:keys [id len] :as car} d-fn]
  (let [street-len (:street.lane.install/length lane)]
    (assoc entity
      :state (conj state (assoc car
                           :front (- street-len len)
                           :dst (d-fn id lane))))))

(defn advance-cars-in-lane [{:keys [state] :as entity}]
  (assoc entity :state (r/reduce (partial advance 1 state) [] state)))

(defn ch->lane [{:keys [channel state] :as entity} d-fn]
  (if-not (zero? (.size channel))
    (add-to-lane entity (take-from-channel channel) d-fn)
    entity))

(defn mark-ripe [{:keys [state] :as entity}]
  (let [[head-car & more] state]
    (if head-car
      (assoc entity :state (conj more (assoc head-car :ripe? (zero? (:front head-car)))))
      entity)))

(defn room-in-lane? [lane car]
  (let [tail-car (last (:state lane))]
    (or (not tail-car)
        (>= (- (:len lane) (back-of-car tail-car)) (:len car)))))

(defn harvest-ingress-lane [{:keys [lane state] :as entity} d-fn elane-snapshot safe?]
  (let [[head-car & more] state
        id (quad lane)]
    (if (and (:ripe? head-car) (safe? id (d-fn (:id head-car) id)))
      (let [out-lane (d-fn (:id head-car) id)
            ch (:channel (elane-snapshot out-lane))]
        (when-not (nil? ch)
          (put-into-ch ch (dissoc head-car :ripe? :front :dst)))
        (assoc entity :state (or more [])))
      entity)))

(defn harvest-egress-lane [{:keys [lane state] :as entity} d-fn ilane-snapshot]
  (let [[head-car & more] state]
    (if (:ripe? head-car)
      (let [id (quad lane)
            in-lane (d-fn (:id head-car) id)]
        (if (room-in-lane? in-lane head-car)
          (let [ch (:channel (ilane-snapshot in-lane))]
            (if-not (nil? ch)
              (put-into-ch ch (dissoc head-car :ripe?))
              (prn head-car "is done driving."))
            (assoc entity :state (or more [])))
          entity))
      entity)))

