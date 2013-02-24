#!/usr/bin/env bash

BASE=$(mktemp -d /tmp/mb.XXXXXX)
CBASE=$(greadlink -f $BASE)

# Example for what happens when a post file changes.
MB="mb -d $CBASE/blog -o $CBASE/html"

$MB -i >/dev/null
touch -t 999901010000 $CBASE/blog/posts/first-post.txt

echo '$ mb'
$MB | sed "s|$CBASE|...|g"

rm -rf $BASE
