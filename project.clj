(defproject MonthGame "1.0.0-SNAPSHOT"
  :description "Game written in a month"
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]
		 [org.clojars.technomancy/jlayer "1.0"]]
  :dev-dependencies [[swank-clojure "1.2.0"]
		     [lein-javac "0.0.2-SNAPSHOT"]]
  :main MonthGame.core)
