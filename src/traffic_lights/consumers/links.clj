(ns traffic-lights.consumers.links
  (:require [compojure.handler :as handler]
            [compojure.response :as response]
            [compojure.core :refer [defroutes GET POST]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.params :refer [wrap-params]]
            [traffic-lights.protocols :as p]
            [traffic-lights.core :refer [storage]]))

(defroutes routes
  (GET "/rush-hour/api/links/edn" {:keys [body]}
       (pr-str {:dsts (p/links storage (read-string (slurp body)))}))
  (POST "/rush-hour/api/reverse-links/edn" {:keys [body] :as x}
        (let [quad (read-string (slurp body))]
          (prn quad)
          (pr-str {:srcs (p/reverse-links storage quad)}))))

(def app (wrap-params #'routes))

(defonce jetty (run-jetty #'app {:port 9091 :join? false}))

