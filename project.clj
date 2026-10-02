(defproject new-seesaw "0.1.1"
  :description "A Swing wrapper/DSL for Clojure with virtual dom. You want seesaw.core, FYI. See http://seesaw-clj.org for more info."

  :url "http://seesaw-clj.org"

  :license {:name         "Eclipse Public License - v 1.0"
            :url          "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments     "same as Clojure"}

  :global-vars {*warn-on-reflection* true}

  ; To run the examples:
  ;
  ;   $ lein examples
  ;
  :aliases {"test" ["run" "-m" "lazytest.main"]
            "examples" ["run" "-m" "seesaw.test.examples.launcher"]}

  :dependencies [[org.clojure/clojure "1.12.6"]
                 [com.miglayout/miglayout-swing "11.4.3"]
                 [com.jgoodies/jgoodies-forms "1.9.0"]
                 [org.swinglabs.swingx/swingx-core "1.6.5-1"]
                 [j18n "1.0.2"]
                 [com.fifesoft/rsyntaxtextarea "4.0.1"]]
  :plugins [[lein-ancient "1.0.0"]
            [dev.weavejester/lein-cljfmt "0.16.6"]]
  :profiles {:dev {:dependencies [[io.github.noahtheduke/lazytest "2.1.0"]
                                  [lein-autodoc "0.9.0"]
                                  [org.openjfx/javafx-swing "27"]]}}
  :autodoc {
            :name       "Seesaw",
            :page-title "Seesaw API Documentation"
            :copyright  "Copyright 2012, Dave Ray"}
  :java-source-paths ["jvm"])

