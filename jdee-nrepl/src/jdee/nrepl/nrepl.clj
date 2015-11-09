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

(defn jdee-get-sourcpath []
  "Return the jdee-sourcepath.  If the process that launches the nREPL server
knows the classpath, it can store it in the jdee.sourceRoots property.  If the
property is not set, returns nil"
  (System/getProperty  "jdee.sourceRoots"))

(defn- find-open-port []
  "Search for an open port"

  (try
    ;; ServerSocket interprets port 0 as "any open port"
    (with-open [server-sock (ServerSocket. 0)]
      (let [port (.getLocalPort server-sock)]
        (println "Found free port: " port)
        ;; Port is free: use this one.
        port))
   (catch IOException exc
     (println "Unable to bind to an open port")
     (throw exc))))

(defn start-server []
  (let [port (find-open-port)
        server
        (clojure.tools.nrepl.server/start-server
         :handler jdee-nrepl-handler
         :port port)]

    (defonce the-server server)
    ;; This message is searched for in jde-interactive.el
    ;; so don't change it without keeping the two in sync
    (println "nREPL server started on port" port)))

(defn say-hi []
  (println "Hello"))

(defn stop-server []
  (println "Stopping server")
  (when (resolve 'the-server)
    (println " server: " the-server)
    (clojure.tools.nrepl.server/stop-server the-server)))

