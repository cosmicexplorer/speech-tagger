speech-tagger
=============

Reads files sent in by lines of stdin and returns json characterizing parts of speech in that file. Uses [corenlp](http://nlp.stanford.edu/software/corenlp.shtml). Intended for use in an emacs extension I'm about to write.

# Install

Clone and run `lein uberjar`, then `java -jar <standalone jar>`, where `<standalone jar>` is the `.jar` file that has "standalone" in the name. I plan for this to be automated in the emacs section.

# License

[GPL](GPL.md)
