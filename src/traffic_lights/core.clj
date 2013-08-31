(ns traffic-lights.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.tools.logging :refer [info]]
            [clojure.set :refer [subset?]]
            [clojure.pprint :refer [pprint]]
            [traffic-lights.queue :as q]))

(defn connections-catalog
  ([q] (connections-catalog (:intersection/of q) (:street/name q)
                            (:street/tag q) (:street.lane.install/name q)))
  ([intx street tag lane]
     (let [ingress (get connections
                        {:src.intersection/of intx
                         :src.street/name street
                         :src.street/tag tag
                         :src.lane/name lane})
           result (queues-index {:intersection/of (:dst.intersection/of ingress)
                                 :street/name (:dst.street/name ingress)
                                 :street/tag (:dst.street/tag ingress)
                                 :street.lane.install/name (:dst.lane/name ingress)})]
       (or result (q/null-gulping-queue)))))

(def intx-catalog (build-non-unique-catalog schema :intersection/of))

(def intx-index (build-catalog schema :intersection/ident))

(def intx-area-index (fmap (fn [_] (ref [])) intx-index))

(defn street-lane-id-index [intx]
  (group-by :street.lane.install/ident
            (map #(select-keys %
                               [:street.lane.install/ident :intersection/of
                                :street/name :street/tag :street.lane.install/name])
                 (intx-catalog intx))))

(defn format-lane [lane-ident]
  (str (:intersection/of lane-ident) "/"
       (:street/tag lane-ident) "/"
       (:street.lane.install/name lane-ident)))

(defn build-light-for-schedule [schedule light-catalog]
  (fmap #(:light-face/init (light-catalog %)) (:schedule/substitute schedule)))

(def traffic-light-index
  (apply merge
         (map (fn [[intx _]]
                {intx (agent (-> intx
                                 intx-index
                                 :intersection.install/schedule
                                 schedule-catalog
                                 (build-light-for-schedule light-catalog)))})
              intx-catalog)))

(defn turn-on-light! [light schedule]
  (future
    (loop []
        (doseq [{:keys [states duration] :as step} (:schedule/sequence schedule)]
          (send light (fn [x] (merge x states)))
          (Thread/sleep duration)))
    (recur)))

(defn semantic-var->4t [vars unevaled-street-map]
  (fmap #(vars %) unevaled-street-map))

(defn evaluate-rule-set [semantic-map rule-set]
  (apply merge (map (fn [sem-var] {sem-var (semantic-map sem-var)})
                    (:lane.rules/vars rule-set))))

(defn registered-rules [rule-sub-catalog street-catalog src]
  (rule-sub-catalog (:street.lane.install/rules (street-catalog src))))

(defn evaluate-rules [evaled-rule-binders]
  (map
   (fn [binder]
     (let [rule (rule-catalog (:lane.rules/register binder))
           sub-map (:lane.rules/substitute binder)]
       (-> rule
           (assoc :src (first (sub-map (:src rule))))
           (assoc :dst (first (sub-map (:dst rule))))
           (assoc :yield (map (fn [[src dst]] [(first (sub-map src)) (first (sub-map dst))]) (:yield rule))))))
   evaled-rule-binders))

(defn evaluate-rule-binders [binders semantic-map]
  (map (fn [binder]
         (assoc binder :lane.rules/substitute
                (fmap (fn [sem-var] (semantic-map sem-var)) (:lane.rules/substitute binder))))
       binders))

(defn applicable-rules [evaled-rules src dst light]
  (filter
   (fn [rule]
     (and (= (dissoc (:src rule) :street.lane.install/ident) src)
          (= (dissoc (:dst rule) :street.lane.install/ident) dst)
          (subset? light (into #{} (:light rule)))))
   evaled-rules))

(defn yield-lanes-clear? [rules]
  (every? true?
          (map
           (fn [rule]
             (let [{:keys [dst yield]} rule]
               (every?
                true?
                (map
                 (fn [[in & out]]
                   (let [in-4t (dissoc in :street.lane.install/ident)]
                     (if-not (empty? (filter identity out))
                       (let [head-car (first (deref (queues-index in-4t)))]
                         (not= (:dst head-car) dst))
                       (lane-is-empty? in-4t))))
                 yield))))
           rules)))

(defn relevant-rules [src dst light]
  (let [vars           (street-lane-id-index (:intersection/of src))
        street-mapping (:street.lane.install/substitute (street-catalog src))
        semantic-map   (semantic-var->4t vars street-mapping)
        binders        (registered-rules rule-substitution-catalog street-catalog src)
        evaled-binders (evaluate-rule-binders binders semantic-map)
        evaled-rules   (evaluate-rules evaled-binders)
        relevant-rules (applicable-rules evaled-rules src dst light)]
    relevant-rules))

(defn yielding-lanes [rules]
  (filter identity
          (mapcat (fn [[a b]]
                    (let [a (dissoc a :street.lane.install/ident)
                          b (dissoc b :street.lane.install/ident)]
                      [(queues-index a) (queues-index b)]))
                  (mapcat :yield rules))))

(defn ignore-light [light me]
  (remove-watch light me))

(defn ignore-yielding-lanes [rules me]
  (doseq [lane (yielding-lanes rules)]
    (remove-watch lane me)))

(defn safe-to-go? [rules]
  (and (not (empty? rules))
       (yield-lanes-clear? rules)))

(defn -main [& args]
  (turn-on-all-traffic-lights!)
  (verbose-queues!)
  (verbose-intersections!)
  (start-all-drivers!))

