(defproject jonure "0.1.0-SNAPSHOT"
  :description "PJON in clojure"
  :url "http://github.com/xlfe/jonure"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [byte-streams "0.2.4"]
                 ;[manifold "0.1.8"]
                 [clojure.java-time "0.3.2"]
                 [clj-serial "2.0.5"]
                 [org.clojure/core.async "0.4.474"]
                 [org.clojure/clojure "1.9.0"]]
  :main jonure.core)

