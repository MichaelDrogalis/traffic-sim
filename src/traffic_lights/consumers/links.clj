(ns traffic-lights.consumers.links
  (:require [compojure.core :refer [defroutes GET POST]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.params :refer [wrap-params]]
            [traffic-lights.protocols :as p]
            [traffic-lights.core :refer [storage]]))

(defroutes routes
  (GET "/rush-hour/api/links/edn" {:keys [body]}
       (pr-str {:dsts (p/links storage (read-string (slurp body)))}))
  (POST "/rush-hour/api/reverse-links/edn" {:keys [body]}
        (let [quad (read-string (slurp body))]
          (pr-str {:srcs (p/reverse-links storage quad)})))
  (POST "/rush-hour/api/lengths/edn" {:keys [body]}
        (let [quads (:quads (read-string (slurp body)))]
          (pr-str
           {:quads
            (into {}
                  (map
                   (fn [x]
                     {x (p/find-lane storage x)})
                   quads))}))))

(def app (wrap-params #'routes))

(defonce jetty (run-jetty #'app {:port 9091 :join? false}))

