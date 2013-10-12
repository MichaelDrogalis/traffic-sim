(ns traffic-lights.consumers.links
  (:require [compojure.core :refer [defroutes GET POST]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.params :refer [wrap-params]]
            [traffic-lights.protocols :as p]
            [traffic-lights.core :refer [storage]]))

(defroutes routes
  (GET "/rush-hour/api/external-links/edn" {:keys [body]}
       (pr-str {:dsts (p/external-links storage (read-string (slurp body)))}))
  (POST "/rush-hour/api/external-reverse-links/edn" {:keys [body]}
        (let [quad (read-string (slurp body))]
          (pr-str {:srcs (p/external-reverse-links storage quad)})))
  (POST "/rush-hour/api/expand-quad/edn" {:keys [body]}
        (let [quad (read-string (slurp body))]
          (pr-str {:quad (p/find-lane storage quad)}))))

(def app (wrap-params #'routes))

; (defonce jetty (run-jetty #'app {:port 9091 :join? false}))


