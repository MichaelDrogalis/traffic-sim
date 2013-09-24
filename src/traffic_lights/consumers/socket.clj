(ns traffic-lights.consumers.socket
  (:require [traffic-lights.util :refer [maph]]
            [org.httpkit.server :refer [send! with-channel] :as s]))

(def listeners (atom #{}))

(defn strip-snapshot [{:keys [lights ingress egress]}]
  {:lights  (maph :state lights)
   :ingress (maph :state ingress)
   :egress  (maph :state egress)})

(defn push-to-clients [snapshot]
  (doseq [client @listeners]
    (send! client (pr-str (strip-snapshot snapshot)))))

(defn handler [request]
  (with-channel request channel
    (s/on-close channel (fn [status] (println "channel closed: " status)))
    (s/on-receive channel (fn [_] (swap! listeners conj channel)))))

(defn watch-queue [queue]
  (add-watch
   queue :logger
   (fn [_ _ _ snapshot]
     (prn listeners)
     (push-to-clients snapshot))))

(s/run-server handler {:port 9090})

