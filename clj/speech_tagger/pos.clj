(ns speech-tagger.pos
  (:import
   (java.io StringReader)
   (java.util ArrayList)
   (edu.stanford.nlp.process PTBTokenizer)
   (edu.stanford.nlp.tagger.maxent MaxentTagger))
  (:require [clojure.data.json :as json]))

(defn tokenize [s]
  (.tokenize (PTBTokenizer/newPTBTokenizer (StringReader. s))))

;;; testing: /models/english-left3words-distsim.tagger
(def load-pos-tagger (memoize (fn [path] (MaxentTagger. path))))

(defn json-read-file [path]
  (json/read-str (slurp path)))

;;; testing: /penn_treebank_tags.json
(def tag-defns (memoize #'json-read-file))

(defn pos-tag [tag-path tokens]
  (map
   (fn [w] [(.word w) (.tag w)])
   (.tagSentence ^MaxentTagger (load-pos-tagger tag-path) ^ArrayList tokens)))

(defn replace-token-modifications [tok]
  "Tokenization changes the actual string sometimes. This reverts the changes."
  (.replaceAll tok "``|''" "\""))

(defn indices-for-tags [string tokens]
  ;; this looks ugly as hell because clojure doesn't allow mutable locals,
  ;; otherwise we could just call map a few times and be done (yes i'm angry
  ;; about this)
  (let [toks-v (vec (map #'replace-token-modifications tokens))
        token-indices
        (loop [cur-index 0
               cur-tok-index 0
               tagged-tok-vec (list)]
          (if (>= cur-tok-index (.length toks-v)) (reverse tagged-tok-vec)
              (let [new-index
                    (.indexOf string (nth toks-v cur-tok-index) cur-index)]
                (recur
                 new-index
                 (inc cur-tok-index)
                 (conj tagged-tok-vec new-index)))))]
    (map (fn [start-index tok] [start-index (+ start-index (.length tok))])
         token-indices toks-v)))

(defn complete-tag-string [str tag-path defns-path]
  "Adds definitions for part of speech and the word itself given the output of
pos-tag and indices-for-tags."
  (map
   (fn [tok-vec]
     (let [[text tag start end] tok-vec
           [tag-defn tag-ex] (get (tag-defns defns-path) tag)]
       {:start start
        :end end
        :text (replace-token-modifications text)
        :tag tag
        :tag-defn tag-defn
        :tag-ex tag-ex}))
   (let [tok-str (tokenize str)]
     (map #'concat (pos-tag tag-path tok-str)
          (indices-for-tags str (map (fn [w] (.word w)) tok-str))))))