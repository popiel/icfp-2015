#!/bin/sh

curl --user :`cat token` -X GET -H "Content-Type: application/json" https://davar.icfpcontest.org/teams/139/solutions

