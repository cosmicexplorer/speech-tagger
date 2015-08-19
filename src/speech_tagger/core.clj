(ns speech-tagger.core
  (:use [speech-tagger.pos])
  (:import (java.io BufferedReader))
  (:gen-class))

(defn -main [tag-path & args]
  (doseq [file args]
    (println (pos-tag tag-path (tokenize (slurp file))))))
