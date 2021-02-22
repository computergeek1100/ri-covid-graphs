#!/bin/bash

cd

cd ri-covid-graphs/scripts

echo > log_cdc.txt

git pull ri-covid-graphs gh-pages

Rscript update_cdc.R

git commit -a -m "CDC data update"

git push
