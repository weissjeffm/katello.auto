(ns katello.client.provision
  (:require [deltacloud :as cloud]           
            (katello [client :as client]
                     [conf :as conf])))


(defn add-ssh [inst]
  (assoc inst :ssh-connection (client/new-runner (cloud/ip-address inst))))

(defmacro with-n-clients
  "Provisions n clients with instance name basename (plus unique
 identifier) and configures them for the katello server under test.
 inst-bind is a symbol or destructuring form to refer to the instance
 data returned from the cloud provider. n is the number of instances
 to provision. Runs body and then terminates the instances (even if
 there was an exception thrown) eg.
   (with-n-clients 2 \"myinstname\" [c1 c2] ...)"
  [n basename inst-bind & body]
  `(cloud/with-instances
       [~inst-bind (let [inst# (update-in (->> (conf/client-defs ~basename)
                                             (take ~n)
                                             (map (partial cloud/provision-all conf/*cloud-conn*))
                                             (map add-ssh))
                                          [:instances]
                                          (partial map-ssh))]
                     (doseq [i# inst#]
                       (client/setup-client (:ssh-connection i#)))
                     inst#)]
     
     ~@body))

(defmacro with-client
  "Provisions a client with instance name clientname (plus unique
 identifier) and configures it for the katello server under test.
 inst-bind is a symbol to refer to the client in the provided body. It
 will be bound to the data returned from the cloud provider. Executes
 body and then terminates the instances (even if there was an
 exception thrown). sample:
   (with-client \"myinstname\" client (do-thing client))"
  [clientname inst-bind & body]
  `(cloud/with-instance
       [~inst-bind (let [inst# (->> (conf/client-defs ~clientname)
                                  first
                                  (cloud/provision conf/*cloud-conn*)
                                  add-ssh)]
                     (client/setup-client (inst# :ssh-connection))
                     inst#)]
     
     ~@body))