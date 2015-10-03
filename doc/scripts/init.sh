#!/usr/bin/env bash
b=$(mktemp -d /tmp/mb.XXXXXX)

# Data directory initialization example.

echo '$ mb -d .../blog -o .../html -i'
../.cabal-sandbox/bin/mb -d $b/blog -o $b/html -i | sed "s|$b|...|g"

rm -rf $b
