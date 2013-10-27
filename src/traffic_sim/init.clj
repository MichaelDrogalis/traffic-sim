(ns traffic-sim.init
  (:require [traffic-sim.core] 
;            [traffic-sim.consumers.logger]
            [traffic-sim.consumers.socket]
            [traffic-sim.consumers.links]
            [traffic-sim.consumers.reload]))

(defn -main [& args]
  (traffic-sim.consumers.links/start-jetty)
  (traffic-sim.consumers.socket/start-socket-server)
  (traffic-sim.core/start-sim))

