(defproject kunstmusik/pink "0.5.0-SNAPSHOT"
  :jvm-opts ;["-server" "-Xmx2g" "-XX:-UseParallelGC"]
  ^:replace
  ["-server"
   "-Xmx512m"           ; Minimum and maximum sizes of the heap
   "-XX:+UseG1GC"
   "-XX:MaxGCPauseMillis=1"     ; Specify a target of 20ms for max gc pauses
   ;"-XX:MaxNewSize=257m"         ; Specify the max and min size of the new
   ;"-XX:NewSize=256m"            ;  generation to be small
   "-XX:+UseTLAB"                ; Uses thread-local object allocation blocks. This
   ;  improves concurrency by reducing contention on
   ;  the shared heap lock.
   ;"-XX:MaxTenuringThreshold=0" ; Makes the full NewSize available to every NewGC 

   ; for GC diagnostics 
   ;"-XX:+PrintGCDetails"
   ;"-XX:+PrintGCTimeStamps"
   ;"-XX:+PrintGCApplicationStoppedTime"
   ;"-verbose:gc"
   ]
  :description "A library for music research, composition, and performance."

  :url "http://github.com/kunstmusik/pink"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.4.474"]
                 [kunstmusik/diff-eq "0.1.2"]]

  :global-vars  {*warn-on-reflection* true}

  :profiles  { 
              :dev  {
                     :dependencies [[criterium "0.4.4"]] 
                     :plugins [[lein-codox "0.10.3"]] 
                     :source-paths ["src/demo"]
                     } 

              :profiling {
                     :plugins [[lein-nodisassemble "0.1.3"]] }      

              :benchmarking {
                             :source-paths ["src/benchmarks"]
                             }      

              :plotting {
                     :dependencies [[incanter "1.9.1"]
                                    ;[kunstmusik/pink-viz "0.1.0-SNAPSHOT"]
                                    ] 
                     :source-paths ["src/plotting"]
                     }
              :1.9 {
                    :dependencies [[org.clojure/clojure "1.9.0"]]
                    }
              }


  :source-paths  ["src/main"] 
  :test-paths  ["src/test"]
  :java-source-paths  ["src/main"] 
  :javac-options     ["-target" "1.7" "-source" "1.7"]
  :scm {:name "git"
        :url "https://github.com/kunstmusik/pink.git" }
  :codox {:source-paths ["src/main"] } 
  ;:main 
  )
