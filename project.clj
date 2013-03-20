(defproject audio-seq "0.1.0-SNAPSHOT"
  :jvm-opts ["-server" "-Xmx2g" "-XX:-UseParallelGC"]
  :description "A library for music research, composition, and performance."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :warn-on-reflection true
  :profiles {
      :dev {
          :warn-on-reflection true
          :dependencies [[criterium "0.3.1"]]}}
  ;:main 
)
