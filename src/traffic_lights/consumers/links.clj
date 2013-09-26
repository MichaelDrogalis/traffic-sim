(ns traffic-lights.consumers.links
  (:require [compojure.handler :as handler]
            [compojure.response :as response]
            [compojure.core :refer [defroutes GET]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.params :refer [wrap-params]]
            [traffic-lights.protocols :as p]))

(defn api [storage]
  (defroutes routes
    (GET "/rush-hour/api/links" {:keys [params]}
         (pr-str {:dsts (p/links storage (read-string (get params "src")))})))

  (def app (wrap-params routes))

  (run-jetty #'app {:port 9091 :join? false}))

