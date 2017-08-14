(defproject dribbble-stats "0.1.0-SNAPSHOT"
  :main dribbble-stats.main
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [manifold "0.1.6"]
                 [aleph "0.4.3"]
                 [cheshire "5.7.1"]
                 [org.clojure/core.async "0.3.443"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [com.cemerick/url "0.1.1"]]
  :profiles {:dev {:resource-paths ["test/resources"]
                   :dependencies [[clj-wiremock "0.3.0"]]}})
