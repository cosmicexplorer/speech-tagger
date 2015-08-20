speech-tagger
=============

Reads files sent in by lines of stdin and returns json characterizing parts of speech in that file. Uses [corenlp](http://nlp.stanford.edu/software/corenlp.shtml). Intended for use in an emacs extension I'm about to write.

# Install

Download [this install script](https://github.com/cosmicexplorer/speech-tagger/releases/download/0.0.0/install.sh), run `chmod +x install.sh`, and run it (you'll need superuser permissions). You can also run curl and pipe to bash, but I don't recommend that route. The `speech-tag` executable should now be on your PATH.

# License

[GPL](GPL.md)
