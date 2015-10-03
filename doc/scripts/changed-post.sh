#!/usr/bin/env bash

function readlink {
    python -c 'import os,sys;print os.path.realpath(sys.argv[1])' "$1"
}

BASE=$(mktemp -d /tmp/mb.XXXXXX)
CBASE=$(readlink $BASE)

# Example for what happens when a post file changes.
MB="../.cabal-sandbox/bin/mb -d $CBASE/blog -o $CBASE/html"

$MB -i >/dev/null
touch -t 999901010000 $CBASE/blog/posts/first-post.txt

echo '$ mb'
$MB | sed "s|$CBASE|...|g"

rm -rf $BASE
