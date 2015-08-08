#!/bin/sh

git status -s | grep -s . && echo "Uncommitted changes and/or untracked files present.  Clean up your mess before submitting!" && exit 1

git tag `date -u +'%Y-%m-%dT%H:%M:%SZ'`
curl --user :`cat token` -X POST -H "Content-Type: application/json" -d @- https://davar.icfpcontest.org/teams/139/solutions

