(defproject clj2048 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.2.4"]
                 [jline "2.11"]]
  :main ^:skip-aot clj2048.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
