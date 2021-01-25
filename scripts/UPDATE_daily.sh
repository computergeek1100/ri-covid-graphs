#!/bin/bash

cd

cd ri-covid-graphs/scripts

echo > log_daily.txt

git pull ri-covid-graphs gh-pages

Rscript dailyGraphs.R

git commit -a -m "Daily graph update"

git push
