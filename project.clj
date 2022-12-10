(defproject serioga/undertow-research "SNAPSHOT"
  :dependencies [;;; Clojure
                 [org.clojure/clojure "1.11.1"]

                 ;;; Web Server
                 [io.undertow/undertow-core "2.3.0.Final"]
                 [ring/ring-core "1.9.6"]
                 [ring/ring-defaults "0.3.4"]

                 ;;; Utils
                 [com.github.strojure/zizzmap "1.0.20"]

                 ;;; Logging
                 [ch.qos.logback/logback-classic "1.2.11" :upgrade false]
                 [ch.qos.logback/logback-core "1.2.11" :upgrade false]
                 [org.clojure/tools.logging "1.2.4"]
                 [org.slf4j/slf4j-api "1.7.36" :upgrade false]]

  :java-source-paths ["src"]
  :javac-options ["-source" "1.8" "-target" "1.8"]

  :profiles {:dev {:dependencies [[luminus/ring-undertow-adapter "1.2.9"]
                                  [org.immutant/web "2.1.10"]]}})
