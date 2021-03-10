#!/bin/bash

cd

cd ri-covid-graphs/scripts

echo > log_owid.txt

git pull ri-covid-graphs gh-pages

Rscript update_owid.R

git commit -a -m "OWID update"

git push
