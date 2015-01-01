(defproject formidable "0.1.3"
  :description "Web forms - rendering, parsing, and validating"
  :url "https://github.com/teamwall/formidable"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [jkkramer/verily "0.6.0"]
                 [clj-time "0.8.0"]
                 [crate "0.2.5"]
                 [prismatic/dommy "1.0.0"]
                 [ring-anti-forgery "0.3.0"]]
  :test-paths ["target/test-classes"]
  :cljx {:builds [{:source-paths ["src"]
                   :output-path "target/classes"
                   :rules :clj}
                  {:source-paths ["src"]
                   :output-path "target/classes"
                   :rules :cljs}
                  {:source-paths ["test"]
                   :output-path "target/test-classes"
                   :rules :clj}
                  {:source-paths ["test"]
                   :output-path "target/test-classes"
                   :rules :cljs}]}
  :prep-tasks [["cljx" "once"]]
  :profiles {:dev {:dependencies [[com.cemerick/clojurescript.test "0.3.3"]
                                  [org.clojure/clojurescript "0.0-2498"]
                                  [com.cemerick/piggieback "0.1.3"]
                                  [com.keminglabs/cljx "0.5.0"]]
                   :plugins [[com.keminglabs/cljx "0.5.0"]]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl
                                                     cljx.repl-middleware/wrap-cljx]}
                   :prep-tasks [["cljx" "once"]]}})
