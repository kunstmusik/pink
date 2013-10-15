(defproject compose "0.1.0-SNAPSHOT"
  :jvm-opts ;["-server" "-Xmx2g" "-XX:-UseParallelGC"]
   ["-Xms512m" "-Xmx1g"           ; Minimum and maximum sizes of the heap
    "-XX:+UseParNewGC"            ; Use the new parallel GC in conjunction with
    "-XX:+UseConcMarkSweepGC"     ;  the concurrent garbage collector
    "-XX:+CMSConcurrentMTEnabled" ; Enable multi-threaded concurrent gc work (ParNewGC)
    "-XX:MaxGCPauseMillis=20"     ; Specify a target of 20ms for max gc pauses
    "-XX:+CMSIncrementalMode"     ; Do many small GC cycles to minimize pauses
    "-XX:MaxNewSize=257m"         ; Specify the max and min size of the new
    "-XX:NewSize=256m"            ;  generation to be small
    "-XX:+UseTLAB"                ; Uses thread-local object allocation blocks. This
                                  ;  improves concurrency by reducing contention on
                                  ;  the shared heap lock.
    "-XX:MaxTenuringThreshold=0"] ; Makes the full NewSize available to every NewGC
  :description "A library for music research, composition, and performance."

  :url "http://example.com/FIXME"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [overtone/at-at "1.2.0"]
                 [prismatic/hiphip "0.1.0"]
                 ]
  :profiles {
             :dev {
                   :warn-on-reflection true
                   :dependencies [[criterium "0.4.2"]]}}

  ;:main 
  )
