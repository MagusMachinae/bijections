(defproject bijections "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.2"]
                 [org.clojure/math.combinatorics "0.3.2"]]
  :profiles {:web-app {:dependencies [[thheller/shadow-cljs "2.28.19"]
                                      [org.clojure/clojurescript "1.11.60"]
                                      [binaryage/devtools "1.0.7"]]}}
  :repl-options {:init-ns bijections.core})
