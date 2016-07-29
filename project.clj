(defproject code-cracker-puzzle "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [net.mikera/core.matrix "0.51.0"]
                 [io.aviso/pretty "0.1.26"]
                 [criterium "0.4.4"]
                 [ring "1.5.0"]
                 [compojure "1.5.1"]
                 [enlive "1.1.6"]
                 [cheshire "5.6.3"]]
  :jvm-opts ^:replace ["-server" "-d64" "-Xmx8g"]
  :plugins [[io.aviso/pretty "0.1.26"]]
  :main ^:skip-aot code-cracker-puzzle.core)

