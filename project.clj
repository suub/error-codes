(defproject suub/error-codes "0.2.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-Xmx4g" "-server" "-Xss1g"]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2322"]
                 [net.mikera/core.matrix "0.17.0"]
                 [gorilla-renderable "1.0.0"]
                 [de.undeadco/marmoset "0.1.0-SNAPSHOT"]
                 [om "0.7.1"]
                 [domina "1.0.2"]
                 [prismatic/dommy "0.1.3"]]
  :plugins [[lein-gorilla "0.3.4-SNAPSHOT"]])
