(ns jdee.nrepl.nrepl
  (:require [clojure.tools.nrepl.server :as nrepl-server]
            [clojure.tools.nrepl
             [middleware :refer [set-descriptor!]]
             [misc :refer [response-for]]
             [transport :as transport]]
            [cider.nrepl.middleware
             classpath
             stacktrace]
            [clojure.java.io :as io])
  (:import (java.net ServerSocket) (java.io IOException)))



(defn jdee-get-sourcepath []
  "Return the jdee-sourcepath.

If the process that launches the nREPL server knows the sourcepath, it can store
it in the jdee.sourceRoots property.  A separate path for test files can be
stored in jdee.testSourceRoots.  If neither property is not set, returns nil"
  (map str (concat (.split (System/getProperty "jdee.sourceRoots")
                           java.io.File/pathSeparator)
                   (.split (System/getProperty "jdee.testSourceRoots")
                           java.io.File/pathSeparator))))


(defn- sourcepath-reply
  [{:keys [transport] :as msg}]
  (transport/send
   transport
   (response-for msg
                 :sourcepath (jdee-get-sourcepath)
                 :status :done)))

(defn wrap-sourcepath
  "Middleware that provides the java sourcepath."
  [handler]
  (fn [{:keys [op] :as msg}]
    (if (= "sourcepath" op)
      (sourcepath-reply msg)
      (handler msg))))

(set-descriptor!
 #'wrap-sourcepath
 {:handles
  {"sourcepath"
   {:doc "Obtain a list of entries in the Java sourcepath."
    :returns {"sourcepath" "A list of the Java sourcepath entries."}}}})

(defn- jdee-get-parent-path []
  "Return the path to the parent, if it exists.

If the process that launches the nREPL server knows the parent location, it can
store it in the jdee.parentPath property.  If the property is not set, returns
nil"
  (System/getProperty "jdee.parentPath"))


(defn- parent-path-reply
  [{:keys [transport] :as msg}]
  (transport/send
   transport
   (response-for msg
                 :parent-path (jdee-get-parent-path)
                 :status :done)))

(defn wrap-parent-path
  "Middleware that provides the parent-path."
  [handler]
  (fn [{:keys [op] :as msg}]
    (if (= "parent-path" op)
      (parent-path-reply msg)
      (handler msg))))

(set-descriptor!
 #'wrap-parent-path
 {:handles
  {"parent-path"
   {:doc "Obtain a the path to the parent module."
    :returns {"parent-path" "The parent module path."}}}})

(defn- jdee-get-child-paths []
  "Return the path to the children, if any exist.

If the process that launches the nREPL server knows the child location(s), it
can store it in the jdee.childPaths property.  If the property is not set,
returns nil"
  (System/getProperty "jdee.childPaths"))


(defn- child-paths-reply
  [{:keys [transport] :as msg}]
  (transport/send
   transport
   (response-for msg
                 :child-paths (jdee-get-child-paths)
                 :status :done)))

(defn wrap-child-paths
  "Middleware that provides the child-paths."
  [handler]
  (fn [{:keys [op] :as msg}]
    (if (= "child-paths" op)
      (child-paths-reply msg)
      (handler msg))))

(set-descriptor!
 #'wrap-child-paths
 {:handles
  {"child-paths"
   {:doc "Obtain the paths to the child modules."
    :returns {"child-paths" "The child module paths."}}}})

(def jdee-middleware
  "A vector of symbols for all the JDEE middleware"
  '[cider.nrepl.middleware.classpath/wrap-classpath
    cider.nrepl.middleware.stacktrace/wrap-stacktrace
    jdee.nrepl.nrepl/wrap-sourcepath
    jdee.nrepl.nrepl/wrap-parent-path
    jdee.nrepl.nrepl/wrap-child-paths
    ]
  )

(def jdee-nrepl-handler
  "JDE's nREPL handler"
  (apply nrepl-server/default-handler
         (map resolve jdee-middleware)))


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

