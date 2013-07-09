(ns katello.client.provision
  (:require [deltacloud :as cloud]
            [slingshot.slingshot :refer [throw+]]
            (katello [client :as client]
                     [conf :as conf]))
  (:import [java.util.concurrent ArrayBlockingQueue TimeUnit]))

(defrecord ProvisionQueue [queue shutdown?])

(def queue (atom nil))

(defn new-queue [capacity]
  (ProvisionQueue. (ArrayBlockingQueue. capacity) nil))

(defn- clean-up-queue [q]
  (let [leftovers (java.util.ArrayList.)]
    (-> q (.drainTo leftovers))
    (doseq [p leftovers]
      (send @p cloud/unprovision))
    (when-not (apply await-for 600000 leftovers)
      (throw+ {:type ::cleanup-timeout
               ::leftover-instance-agents leftovers}))))

(defn fill-queue
  [pqa]
  (let [provision #(cloud/provision conf/*cloud-conn* %)]
    (loop [defs (conf/client-defs "pre-provision")]
      (let [d (first defs)
            p (promise)]
        (println p)
        (while (not (or (:shutdown? @pqa)
                        (.offer (:queue @pqa) p 5 TimeUnit/SECONDS)))
          (println "offered")) 
        (if (:shutdown? @pqa)
          (clean-up-queue (:queue @pqa))
          (do
            (println "running future")
            (future (try (deliver p (agent (provision d)))
                         (catch Exception e
                           (deliver p e))))
            (recur (rest defs))))))))


(defn add-ssh [inst]
  (try
    (assoc inst :ssh-connection
           (client/new-runner (cloud/ip-address inst)))
    (catch Exception e
      (assoc inst :ssh-connection-error e))))



(defmacro with-n-clients
  "Provisions n clients with instance name basename (plus unique
 identifier) and configures them for the katello server under test.
 inst-bind is a symbol or destructuring form to refer to the instance
 data returned from the cloud provider. n is the number of instances
 to provision. Runs body and then terminates the instances (even if
 there was an exception thrown) eg.
   (with-n-clients 2 \"myinstname\" [c1 c2] ...)"
  [n clientname ssh-conns-bind & body]
  `(cloud/with-instances
     [inst# (->> (conf/client-defs ~clientname)
               (take ~n)
               (cloud/provision-all conf/*cloud-conn*)
               :instances
               (pmap add-ssh))]
     (let [~ssh-conns-bind (do (doall
                                (pmap 
                                 (comp client/setup-client :ssh-connection) inst#))
                               (map :ssh-connection inst#))]
       ~@body)))

(defmacro with-client
  "Provisions a client with instance name clientname (plus unique
 identifier) and configures it for the katello server under test.
 ssh-conn-bind will be bound to an SSHCommandRunner to run commands on
 the client. Executes body and then terminates the instances (even if
 there was an exception thrown). sample:
   (with-client \"myinstname\" client (do-thing client))"
  [clientname ssh-conn-bind & body]
  `(cloud/with-instance
       [inst# (->> (conf/client-defs ~clientname)
                 first
                 (cloud/provision conf/*cloud-conn*)
                 add-ssh)]
     
     (let [~ssh-conn-bind (:ssh-connection inst#)]
       (client/setup-client ~ssh-conn-bind (:name inst#))
       ~@body)))

(defmacro with-queued-client
  [ssh-conn-bind & body]
  `(let [agnt# (-> queue
                   deref ; the atom
                   :queue
                   .poll
                   deref) ; the promise
         ~ssh-conn-bind (client/new-runner (cloud/ip-address (deref agnt#)))]
     (client/setup-client ~ssh-conn-bind (-> agnt# deref :name))
     ~@body
     (send agnt# cloud/unprovision)))
