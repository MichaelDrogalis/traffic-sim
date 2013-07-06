(ns traffic-lights.core
  (:require [dire.core :refer [with-precondition!]]))

(defn legal-init-state? [states init-vals]
  (every? (fn [x] (some #{x} states)) init-vals))

(defn has-face? [light]
  (contains? light :light/face))

(defn has-states? [light]
  (contains? light :light/states))

(defn has-init? [light]
  (contains? light :light/init))

(defn face-type-valid? [face]
  (= (type face) clojure.lang.Keyword))

(defn states-type-valid? [states]
  (= (type states) clojure.lang.PersistentVector))

(defn init-type-valid? [init]
  (= (type init) clojure.lang.PersistentVector))

(defn non-empty-states? [states]
  (> (count states) 0))

(defn non-empty-init? [init]
  (> (count init) 0))

(defn only-keywords? [coll]
  (every? (partial = clojure.lang.Keyword) (map type coll)))

(defn states-only-keywords? [states]
  (only-keywords? states))

(defn init-only-keywords? [init]
  (only-keywords? init))

(defn process-face [face]
  )

(with-precondition! #'process-face :face-prescence has-face?)
(with-precondition! #'process-face :state-presence has-states?)
(with-precondition! #'process-face :init-presence has-init?)

(with-precondition! #'process-face :face-type #(-> % :light/face face-type-valid?))
(with-precondition! #'process-face :states-type #(-> % :light/states states-type-valid?))
(with-precondition! #'process-face :init-type #(-> % :light/init init-type-valid?))

(with-precondition! #'process-face :non-empty-states #(-> % :light/states non-empty-states?))
(with-precondition! #'process-face :non-empty-init #(-> % :light/init non-empty-init?))

(with-precondition! #'process-face :state-keywords #(-> % :light/states states-only-keywords?))
(with-precondition! #'process-face :init-keywords #(-> % :light/init init-only-keywords?))

