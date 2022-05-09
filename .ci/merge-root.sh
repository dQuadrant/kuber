#!/usr/bin/env bash
set -eo pipefail;

### Author github.com/mesudip
### Copies files from a chroot isolation directory to root directory.

for FILE in $( (cd $1 ;find . -type f ) | cut -c 2- )
 do
  if [ ! -f $FILE ]
  then 
    echo Creating $FILE
    mv "$1$FILE" $FILE
  fi
done