#!/bin/bash

cd

cd ri-covid-graphs/scripts

echo > log.txt

git pull ri-covid-graphs gh-pages

Rscript update_graphs.R

git commit -a -m "Daily update"

git push
