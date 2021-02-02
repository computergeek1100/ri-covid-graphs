#!/bin/bash

cd

cd ri-covid-graphs/scripts

echo > sh-log.txt

git pull ri-covid-graphs gh-pages

Rscript dailyGraphs.R

Rscript vaxGraph.R

cd ../graphs/plotlyJS/plotly-htmlwidgets-css-1.57.1

cmp plotly-css-BACKUP.css plotly-htmlwidgets.css
status = $?
if [$status != 0]
then
cp plotly-css-BACKUP.css plotly-htmlwidgets.css
fi

cd ../../../
git commit -a -m "Daily graph update"

git push
