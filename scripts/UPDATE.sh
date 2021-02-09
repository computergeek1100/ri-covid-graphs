#!/bin/bash

cd

cd ri-covid-graphs/scripts

echo > log.txt

git pull ri-covid-graphs gh-pages

Rscript dailyGraphs.R

Rscript vaxGraph.R

git commit -a -m "Daily graph update"

git push
