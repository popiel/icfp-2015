#!/bin/sh

git status -s | grep -s . && echo "Uncommitted changes and/or untracked files present.  Clean up your mess before submitting!" && exit 1

git archive HEAD | gzip > /tmp/invisibleimp.tar.gz
curl --user :`cat token` -X POST -H "Content-Type: application/x-compressed" --data-binary @/tmp/invisibleimp.tar.gz https://davar.icfpcontest.org/teams/139/sourcecode

