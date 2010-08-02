#!/bin/bash

set -e

#SKEL=$(dirname $0)/../skel
SKEL=$(dirname $0)/../blog
EQN2IMG_NORMAL="eqn2img -r 120 -s 10"

echo '\Leftarrow' | $EQN2IMG_NORMAL -c 0000FF -b FFFFFF > $SKEL/html/images/newer.png
echo
echo '\Leftarrow' | $EQN2IMG_NORMAL -c AAAAAA -b FFFFFF > $SKEL/html/images/newer-subdued.png
echo
echo '\Rightarrow' | $EQN2IMG_NORMAL -c 0000FF -b FFFFFF > $SKEL/html/images/older.png
echo
echo '\Rightarrow' | $EQN2IMG_NORMAL -c AAAAAA -b FFFFFF > $SKEL/html/images/older-subdued.png
echo
