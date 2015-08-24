(ns speech-tagger.pos
  (:import
   (java.io StringReader)
   (java.util ArrayList)
   (edu.stanford.nlp.process PTBTokenizer)
   (edu.stanford.nlp.tagger.maxent MaxentTagger)
   (edu.stanford.nlp.util StringUtils))
  (:require
   [clojure.data.json :as json]
   [clojure.java.io :as io]))

(defn tokenize [s]
  (.tokenize (PTBTokenizer/newPTBTokenizer (StringReader. s))))

(def load-pos-tagger
  (memoize
   (fn []
     (let [model-file
           (.toString (io/resource "english-left3words-distsim.tagger"))]
       ;; copied from corenlp's source; doesn't seem to be a way to turn off
       ;; that stupid loading message otherwise
       (MaxentTagger. model-file
                      (StringUtils/argsToProperties
                       ;; variadic args from java to clojure are dumb
                       ;; especially with functions of multiple arity
                       (into-array ^String ["-model" model-file]))
                      false)))))

(defn pos-tag [tokens]
  (map
   (fn [w] [(.word w) (.tag w)])
   (.tagSentence ^MaxentTagger (load-pos-tagger) ^ArrayList tokens)))

(defn replace-token-modifications [tok]
  "Tokenization changes the actual string sometimes. This reverts the changes."
  (.replaceAll tok "``|''" "\""))

(defn indices-for-tags [string tokens]
  ;; this looks ugly as hell because clojure doesn't have mutable locals,
  ;; otherwise we could just call map a few times and be done (yes i'm angry
  ;; about this)
  (let [toks-v (vec (map #'replace-token-modifications tokens))
        token-indices
        (loop [cur-index 0 cur-tok-index 0 tagged-tok-vec (list)]
          (if (>= cur-tok-index (.length toks-v)) (reverse tagged-tok-vec)
              (let [new-index
                    (.indexOf string (nth toks-v cur-tok-index) cur-index)]
                (recur new-index (inc cur-tok-index)
                       (conj tagged-tok-vec new-index)))))]
    (map (fn [start-index tok] [start-index (+ start-index (.length tok))])
         token-indices toks-v)))

(defn complete-tag-string [str]
  "Adds definitions for part of speech and the word itself given the output of
pos-tag and indices-for-tags."
  (map
   (fn [tok-vec]
     (let [[text tag start end] tok-vec]
       {:start start
        :end end
        :text (replace-token-modifications text)
        :tag tag}))
   (let [tok-str (tokenize str)]
     (map #'concat (pos-tag tok-str)
          (indices-for-tags str (map (fn [w] (.word w)) tok-str))))))
