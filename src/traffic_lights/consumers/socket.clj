(ns traffic-lights.consumers.socket
  (:require [traffic-lights.util :refer [maph]])
  (:import [org.webbitserver WebServer WebServers WebSocketHandler]))

(def listeners (atom #{}))

(defn strip-snapshot [{:keys [lights ingress egress]}]
  {:lights  (maph :state lights)
   :ingress (maph :state ingress)
   :egress  (maph :state egress)})

(defn push-to-clients [snapshot]
  (doseq [channel @listeners]
    (.send channel (pr-str {:snapshot (strip-snapshot snapshot)}))))

(defn watch-queue [queue]
  (add-watch
   queue :logger
   (fn [_ _ _ snapshot]
     (prn listeners)
     (push-to-clients snapshot))))

(doto (WebServers/createWebServer 9090)
    (.add "/rush-hour/stream/edn"
          (proxy [WebSocketHandler] []
            (onOpen [chan] (swap! listeners conj chan))
            (onClose [chan] (swap! listeners disj chan))
            (onMessage [_])))
    (.start))

