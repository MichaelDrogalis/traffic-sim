(ns traffic-lights.core
  (:require [clojure.core.async :refer [chan go >!! <!! <! >!]]
            [clojure.algo.generic.functor :refer [fmap]]
            [clojure.set :refer [subset?]]
            [clojure.tools.logging :refer [info]]
            [clojure.pprint :refer [pprint]]))

(defprotocol Touch
  (touch [this]))

(extend-protocol Touch
  clojure.lang.Ref
  (touch [x] (dosync (commute x identity)))
  clojure.lang.Agent
  (touch [x] (send x identity)))

(def schema
  (read-string (slurp (clojure.java.io/resource "intersection-schema.edn"))))

(defn ensure-uniqueness [catalog]
  (fmap first catalog))

(defn group-by-key [schema kw]
  (group-by kw (filter #(contains? % kw) schema)))

(defn build-catalog [schema kw]
  (ensure-uniqueness (group-by-key schema kw)))

(defn build-non-unique-catalog [schema kw]
  (group-by-key schema kw))

(defn build-composite-key-catalog [schema id-kw kws]
  (group-by #(select-keys % kws) (filter #(contains? % id-kw) schema)))

(def light-catalog (build-catalog schema :light-face/ident))

(def schedule-catalog (build-catalog schema :schedule/ident))

(def rule-catalog (build-catalog schema :rule/ident))

(def lane-rules (build-catalog schema :lane.rules/ident))

(def rule-substitution-catalog (build-non-unique-catalog schema :lane.rules/of))

(def street-catalog
  (ensure-uniqueness (build-composite-key-catalog
                      schema :intersection/of
                      [:intersection/of
                       :street/name
                       :street/tag 
                       :street.lane.install/name])))

(def queues-index
  (fmap (fn [_] (ref [])) street-catalog))

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
  (str (:street/tag lane-ident) "/"
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

(defn lane-is-empty? [src]
  (empty? @(queues-index src)))

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

(defn drive-through-intersection [me]
  (info (:id @me) "is driving through the intersection.")
  (dosync (alter (intx-area-index (:intersection/of (:src @me))) conj me))
  (dosync
   (alter (intx-area-index (:intersection/of (:src @me))) (partial filter (partial not= me)))
   (alter (queues-index (:src @me)) #(vec (filter (partial not= me) %)))
   (send me dissoc :src)))

(defn wait-for-light [light me ch]
  (add-watch light me
             (fn [_ _ old new]
               (when-not (= old new)
                 (go (>! ch light))))))

(defn yielding-lanes [rules]
  (filter identity
          (mapcat (fn [[a b]]
                    (let [a (dissoc a :street.lane.install/ident)
                          b (dissoc b :street.lane.install/ident)]
                      [(queues-index a) (queues-index b)]))
                  (mapcat :yield rules))))

(defn watch-yielding-lanes [rules me ch]
  (doseq [lane (yielding-lanes rules)]
    (add-watch lane me
               (fn [_ _ old new]
                 (when-not (= old new)
                   (go (>! ch true)))))))

(defn ignore-light [light me]
  (remove-watch light me))

(defn ignore-yielding-lanes [rules me]
  (doseq [lane (yielding-lanes rules)]
    (remove-watch lane me)))

(defn safe-to-go? [rules]
  (and (not (empty? rules))
       (yield-lanes-clear? rules)))

(defn wait-for-turn [me]
  (let [{:keys [src dst]} @me
        light-ident (:street.lane.install/light (street-catalog src))
        light (traffic-light-index (:intersection/of src))
        ch (chan)]
    (go (loop []
          (let [rules (relevant-rules src dst (light-ident (deref light)))]
            (if (safe-to-go? rules)
              (drive-through-intersection me)
              (do (wait-for-light light me ch)
                  (watch-yielding-lanes rules me ch)
                  (touch light)
                  (doseq [x (yielding-lanes rules)] (touch x))
                  (<! ch)
                  (ignore-light light me)
                  (ignore-yielding-lanes rules me)
                  (recur))))))))

(defn watch-car-ahead-of-me [target me ch]
  (add-watch target me
             (fn [_ _ _ car]
               (when-not (= (:src car) (:src @me))
                 (do (remove-watch target me)
                     (go (>! ch true)))))))

(defn drive-to-ingress-lane [me]
  (let [queue (queues-index (:src @me))]
    (let [tail (peek @queue)]
      (dosync (alter queue conj me))
      (if (> (count @queue) 1)
        (let [ch (chan)]
          (watch-car-ahead-of-me tail me ch)
          (touch tail)
          (go (<! ch)
              (wait-for-turn me)))
        (wait-for-turn me)))))

(defn echo-light-state [light]
  (add-watch light :printer
             (fn [_ _ old new]
               (when-not (= old new)
                 (prn new)))))

(defn turn-on-all-traffic-lights! []
  (doseq [[intx light] traffic-light-index]
    (echo-light-state light)
    (turn-on-light! light (schedule-catalog (:intersection.install/schedule (intx-index intx))))))

(defn verbose-queues! []
  (doseq [[k q] queues-index]
    (add-watch q :printer
               (fn [_ _ old new]
                 (when-not (= old new)
                   (info (format-lane k) ":" (map (comp :id deref) new)))))))

(defn verbose-intersections! []
  (doseq [[k a] intx-area-index]
    (add-watch a :printer (fn [_ _ _ area] (info k "::" (map (comp :id deref) area))))))

(defn start-all-drivers! []
  (doseq [driver (read-string (slurp (clojure.java.io/resource "drivers.edn")))]
    (drive-to-ingress-lane (agent driver))))

(defn -main [& args]
  (turn-on-all-traffic-lights!)
  (verbose-queues!)
  (verbose-intersections!)
  (start-all-drivers!))

