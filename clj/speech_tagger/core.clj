(ns speech-tagger.core
  (:require
   [speech-tagger.pos :as pos]
   [clojure.data.json :as json])
  (:import (java.io BufferedReader))
  (:gen-class))

(defn analyze-file [file-path tag-path defns-path]
  "Analyzes the contents of a file according to pos/complete-tag-string. Meant
to be used by text editors exporting SMALL temporary files. Large files may
cause the tagger to cry."
  {:file-path file-path
   :tagged (pos/complete-tag-string (slurp file-path) tag-path defns-path)})

(def begin-string "Successfully loaded!")

(defn print-err [msg] (.println *err* msg))

(def get-home (memoize (fn [] (System/getenv "HOME"))))

(defn tilde-expand [str] (.replace str "~" (get-home)))

(defn -main [tag-path-unfiltered defns-path-unfiltered & args]
  ;; these are memoized, so we're not throwing away data. this also checks if
  ;; the files actually exist immediately upon startup. these are still memoized
  ;; and not set as globals for easier testing
  ;; this will spew something out to stderr, really wish corenlp would stop that
  (let [tag-path (tilde-expand tag-path-unfiltered)
        defns-path (tilde-expand defns-path-unfiltered)]
    (pos/load-pos-tagger tag-path)
    (pos/tag-defns defns-path)
    (print-err (format "%s %s" "Tagger loaded from:" tag-path))
    (print-err (format "%s %s" "Tag definitions loaded from:" defns-path))
    (print-err begin-string)
    (doseq [temp-file (line-seq (BufferedReader. *in*))]
      (println
       (json/write-str
        (analyze-file (tilde-expand temp-file) tag-path defns-path))))))
