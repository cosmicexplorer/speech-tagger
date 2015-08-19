(ns speech-tagger.pos
  (:import
   (java.io StringReader)
   (java.util ArrayList)
   (edu.stanford.nlp.process PTBTokenizer)
   (edu.stanford.nlp.tagger.maxent MaxentTagger))
  (:require [clojure.data.json :as json]))

(defn tokenize [s]
  (.tokenize
   (PTBTokenizer/newPTBTokenizer
    (StringReader. s))))

;;; testing: /models/english-left3words-distsim.tagger
(def load-pos-tagger
  (memoize
   (fn [path] (MaxentTagger. path))))

(defn json-read-file [path]
  (json/read-str (slurp path)))

;;; in testing, it's /penn_treebank_tags.json
(def tag-defns (memoize #'json-read-file))

(defn pos-tag [tag-path tokens]
  (map
   (fn [w] [(.word w) (.tag w)])
   (.tagSentence ^MaxentTagger (load-pos-tagger tag-path) ^ArrayList tokens)))

(defn complete-tag-string [str tag-path defns-path]
  "Adds definitions for part of speech and the word itself given the output of
pos-tag."
  (map
   (fn [tok-vec]
     (let [[word tag] tok-vec
           [tag-defn tag-ex] (get (tag-defns defns-path) tag)]
       {:word word
        :tag tag
        :tag-defn tag-defn
        :tag-ex tag-ex}))
   (pos-tag tag-path (tokenize str))))
