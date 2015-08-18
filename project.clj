(defproject speech-tagger "0.0.0"
  :description "tag parts of speech with stanford nlp"
  :url "https://github.com/cosmicexplorer/speech-tagger"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [edu.stanford.nlp/stanford-corenlp "3.5.2"]]
  :aot (speech-tagger.core)
  :main speech-tagger.core)
