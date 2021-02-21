#!/bin/bash

cd

cd ri-covid-graphs/scripts

echo > log.txt

git pull ri-covid-graphs gh-pages

Rscript update_ridoh.R

git commit -a -m "RIDOH data update"

git push
