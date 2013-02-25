#!/usr/bin/env bash

function readlink {
    python -c 'import os,sys;print os.path.realpath(sys.argv[1])' "$1"
}

BASE=$(mktemp -d /tmp/mb.XXXXXX)
CBASE=$(readlink $BASE)

# Listen mode example: changes: post index, then the blog config file,
# then one of the posts.
MB="mb -d $CBASE/blog -o $CBASE/html"

SEDPAT="s|$CBASE|...|g"

$MB -i >/dev/null

(
    echo '$ mb -l'
    $MB -l &

    sleep 2

    touch $CBASE/blog/posts/posts-index
    sleep 2

    touch $CBASE/blog/blog.cfg
    sleep 2

    touch $CBASE/blog/posts/first-post.txt
    sleep 2

    kill %1
) | sed $SEDPAT

rm -rf $BASE
