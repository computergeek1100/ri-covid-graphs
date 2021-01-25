#!/bin/bash

cd

cd ri-covid-graphs/scripts

echo > log_vax.txt

git pull ri-covid-graphs gh-pages

Rscript vaxGraph.R

git commit -a -m "Vaccine graph update"

git push
