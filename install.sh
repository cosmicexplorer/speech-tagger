#!/bin/sh

gh_pfx="https://github.com/cosmicexplorer/speech-tagger/releases/download"
version="0.0.0"
jar_url="$gh_pfx/$version/speech-tagger-0.0.0-SNAPSHOT-standalone.jar"
model_url="$gh_pfx/$version/english-left3words-distsim.tagger"
tags_url="$gh_pfx/$version/penn_treebank_tags.json"
bindir="/usr/bin/speech-tagger-lib"

mkdir -p "$bindir"
for url in "$jar_url" "$model_url" "$tags_url"; do
  wget "$url" -P "$bindir"
done

cat >/usr/bin/speech-tag <<EOF
#!/bin/sh
prefix="$bindir"
java -jar "\$prefix/speech-tagger-0.0.0-SNAPSHOT-standalone.jar" \
  "\$prefix/english-left3words-distsim.tagger" \
  "\$prefix/penn_treebank_tags.json"
EOF
chmod +x /usr/bin/speech-tag
