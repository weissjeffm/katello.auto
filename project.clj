(defproject katello "1.0.0-SNAPSHOT"
  :description "Katello automation"  
  :main katello.tests.suite
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [webui-framework "1.0.2-SNAPSHOT"]
                 [test.tree "0.3.0-SNAPSHOT"]
                 [error.handler "1.1.0-SNAPSHOT"]
                 [inflections "0.4"]
                 [clj-http "0.1.3"]
                 [robert/hooke "1.1.1"]
                 [fn.trace "1.0.0-SNAPSHOT"]]
  :jvm-opts ["-Xmx384m"]
  :repositories {"clojars" {:url "http://clojars.org/repo"
                            :snapshots {:update :always}}})

(comment "Execute this in the repl to load everything and start selenium"
         (do
           (do (require 'katello.tasks :reload-all)
               (require 'katello.conf :reload)
               (require 'katello.tests.setup :reload)
                 
               (katello.conf/init))  ;;<-here for api only
           (katello.tests.setup/new-selenium true)
           (katello.tests.setup/start-selenium))   ;;<-here for selenium
         )
