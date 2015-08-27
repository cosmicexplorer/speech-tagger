speech-tagger
=============

The [clojure](clj/speech_tagger) reads json in stdin and returns json characterizing parts of speech in that text using [corenlp](http://nlp.stanford.edu/software/corenlp.shtml). This is used in an [emacs extension](emacs/) (MELPA link incoming when done).

# Install

Clone and run `lein uberjar`, then `java -jar <standalone jar>`, where `<standalone jar>` is the `.jar` file that has "standalone" in the name. I plan for this to be automated in the emacs section.

# Why?

Honestly? I was bored. I've taken enough language classes to be able to characterize parts of speech without a computer by my side. Maybe I'll think of a use for nlp integration in text editors at some point in the future.

A friend of mine said it might be cool if I was to tag parts of speech in a separate language, then offer a translation of that text. It might be pretty cool to offer that, in addition to a google translation of the text. Tagging parts of speech in all the languages corenlp supports might be useful for teaching?

# License

[GPL](GPL.md)
