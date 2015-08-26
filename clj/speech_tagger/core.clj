(ns speech-tagger.core
  (:require
   [speech-tagger.pos :as pos]
   [clojure.data.json :as json])
  (:import (java.io BufferedReader))
  (:gen-class))

(defn analyze-job [job-id string]
  "Analyzes the contents of a string according to pos/complete-tag-string. Meant
to be used by text editors exporting SMALL strings. Large strings may cause the
tagger to cry."
  {:job-id job-id
   :tagged-string (pos/complete-tag-string string)})

(defn -main [& args]
  (doseq [editor-job (line-seq (BufferedReader. *in*))]
    (when (re-find #"[^\s]" editor-job) ; if not empty or whitespace
      (let [{:keys [job-id string]} (json/read-str editor-job :key-fn keyword)]
        (println (json/write-str (analyze-job job-id string)))))))
