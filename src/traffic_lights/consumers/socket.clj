(ns traffic-lights.consumers.socket
  (:require [traffic-lights.util :refer [maph]]
            [traffic-lights.core :refer [queue]])
  (:import [org.webbitserver WebServer WebServers WebSocketHandler]))

(def listeners (atom #{}))

(defn strip-snapshot [{:keys [lights ingress egress]}]
  {:lights  (maph :state lights)
   :ingress (maph :state ingress)
   :egress  (maph :state egress)})

(defn push-to-clients [snapshot]
  (doseq [channel @listeners]
    (.send channel (pr-str {:snapshot (strip-snapshot snapshot)}))))

(add-watch
 queue :socket
 (fn [_ _ _ snapshot]
   (prn listeners)
   (push-to-clients snapshot)))

(defn start-socket-server []
  (doto (WebServers/createWebServer 9090)
    (.add "/rush-hour/streaming/edn"
          (proxy [WebSocketHandler] []
            (onOpen [chan] (swap! listeners conj chan))
            (onClose [chan] (swap! listeners disj chan))
            (onMessage [_])))
    (.start)))

