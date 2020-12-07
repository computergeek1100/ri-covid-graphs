#!/bin/bash

git fetch ri-covid-graphs gh-pages

git pull ri-covid-graphs gh-pages

cd /home/pi/ri-covid-graphs/R

Rscript DailyGraphs.R

Rscript Weekly100k.R

git commit -a -m "Graph update"

git push