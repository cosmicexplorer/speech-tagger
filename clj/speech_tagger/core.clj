(ns speech-tagger.core
  (:require
   [speech-tagger.pos :as pos]
   [clojure.data.json :as json])
  (:import (java.io BufferedReader))
  (:gen-class))

(defn analyze-job [string job-id]
  "Analyzes the contents of a string according to pos/complete-tag-string. Meant
to be used by text editors exporting SMALL strings. Large strings may cause the
tagger to cry."
  {:job-id job-id
   :tagged-string (pos/complete-tag-string string)})

(defn -main [& args]
  (doseq [job-from-editor (line-seq (BufferedReader. *in*))]
    (let [{:keys [job-id string]} (json/read-str job-from-editor)]
      (println (json/write-str (analyze-job string job-id))))))
