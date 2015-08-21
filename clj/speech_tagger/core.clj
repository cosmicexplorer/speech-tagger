(ns speech-tagger.core
  (:require
   [speech-tagger.pos :as pos]
   [clojure.data.json :as json])
  (:import (java.io BufferedReader))
  (:gen-class))

(defn analyze-file [file-path]
  "Analyzes the contents of a file according to pos/complete-tag-string. Meant
to be used by text editors exporting SMALL temporary files. Large files may
cause the tagger to cry."
  {:file-path file-path
   :tagged (pos/complete-tag-string (slurp file-path))})

(def begin-string "Successfully loaded!")

(defn print-err [msg] (.println *err* msg))

(defn tilde-expand [str] (.replace str "~" (System/getenv "HOME")))

(defn -main [& args]
  (doseq [temp-file (line-seq (BufferedReader. *in*))]
    (println (json/write-str (analyze-file (tilde-expand temp-file))))))
