(ns jdee.nrepl.nrepl
  (:require [clojure.tools.nrepl.server :as nrepl-server]
            [cider.nrepl.middleware
             classpath
             stacktrace]
            [clojure.java.io :as io])
  (:import (java.net ServerSocket) (java.io IOException)))


(def jdee-middleware
  "A vector of symbols for all the JDEE middleware"
  '[cider.nrepl.middleware.classpath/wrap-classpath
    cider.nrepl.middleware.stacktrace/wrap-stacktrace
    ]
  )

(def jdee-nrepl-handler
  "JDE's nREPL handler"
  (apply nrepl-server/default-handler
         (map resolve jdee-middleware)))

(defn- find-open-port [port]
  "Search for an open port, starting at PORT"

  (try
    (with-open [server-sock (ServerSocket. port)]
      ;; Port is free: use this one.
      port)
   (catch IOException exc
     ;; Port is already in use, try the next one
     (println "Port " port " is already in use, trying next port")
     (find-open-port (+ port 1)))))

(defn start-server []
  (let [port (find-open-port 12345)
        server
        (clojure.tools.nrepl.server/start-server
         :handler jdee-nrepl-handler
         :port port)]
    (defonce the-server server)
    ;; This message is searched for in jde-interactive.el
    ;; so don't change it without keeping the two in sync
    (println "nREPL server started on port" port)))

(defn stop-server []
  (when (bound? the-server)
    (clojure.tools.nrepl.server/stop-server the-server)))

