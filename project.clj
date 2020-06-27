(defproject gobook "0.1.0-SNAPSHOT"
  :description "A collection of tools for writing Go books."
  :url "http://github.com/rbuchmann/gobook"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [instaparse "1.4.10"]]

  :repl-options {:init-ns gobook.core})
