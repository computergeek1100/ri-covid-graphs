#!/bin/bash

echo > /home/pi/ri-covid-graphs/log.txt

cd /home/pi/ri-covid-graphs/

git fetch ri-covid-graphs gh-pages

git pull ri-covid-graphs gh-pages

cd /R

Rscript dailyGraphs.R

cd ../

git commit -a -m "Graph update"

git push
