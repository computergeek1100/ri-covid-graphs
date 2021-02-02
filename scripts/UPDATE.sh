#!/bin/bash

cd

cd ri-covid-graphs/scripts

echo > sh-log.txt

git pull ri-covid-graphs gh-pages

Rscript dailyGraphs.R

Rscript vaxGraph.R

cd ../graphs/plotlyJS/plotly-htmlwidgets-css-1.57.1

cp plotly-css-BACKUP.css plotly-htmlwidgets.css

cd ../../../
git commit -a -m "Daily graph update"

git push
