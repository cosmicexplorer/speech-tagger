(defproject speech-tagger "0.0.0-SNAPSHOT"
  :description "tag parts of speech with stanford nlp"
  :url "https://github.com/cosmicexplorer/speech-tagger"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.json "0.2.6"]
                 [edu.stanford.nlp/stanford-corenlp "3.5.2"]]
  :aot :all
  :main speech-tagger.core
  :source-paths ["clj"]
  ;; nlp takes up lotsa memory
  :jvm-opts ["-Xmx1g"])
