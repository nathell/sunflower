(defproject sunflower "0.1.0"
  :description "Extract pure texts from HTML collections."
  :main pl.danieljanus.sunflower
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [com.miglayout/miglayout "3.7.2"]
                 [hiccup "0.2.3"]
                 [flyingsaucer/flyingsaucer "8.0"]
                 [clj-iter "0.1.0-SNAPSHOT"]
                 [commons-io "1.4"]
                 [clj-tagsoup "0.1.1"]]
  :dev-dependencies [[leiningen/lein-swank "1.1.0"]])
