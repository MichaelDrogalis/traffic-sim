(ns traffic-lights.init
  (:require [traffic-lights.core] 
;            [traffic-lights.consumers.logger]
            [traffic-lights.consumers.socket]
            [traffic-lights.consumers.links]
            [traffic-lights.consumers.reload]))

(defn -main [& args]
  (traffic-lights.consumers.links/start-jetty)
  (traffic-lights.consumers.socket/start-socket-server)
  (traffic-lights.core/start-sim))

