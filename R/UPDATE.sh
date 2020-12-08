#!/bin/bash

echo > /home/pi/ri-covid-graphs/log.txt

cd /home/pi/ri-covid-graphs/R

git fetch ri-covid-graphs gh-pages

git pull ri-covid-graphs gh-pages

Rscript DailyGraphs.R

git commit -a -m "Graph update"

git push