library(tidyverse)
library(plotly)
library(ggthemes)
library(lubridate)
library(htmlwidgets)
library(googlesheets4)

rateData <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4/edit#gid=1592746937", sheet = "Rate Trends by Age")
dateUpdated <- rateData[[2,2]]
print(dateUpdated)
  
rateDataCleaned <- rateData%>%
  select(weekEnding=1,casesPer100k=14)%>%
  filter(row_number() >= 8)
rateDataCleaned$casesPer100k <- as.numeric(as.character(rateDataCleaned$casesPer100k))
rateDataCleaned$weekEnding <- sub("\\d{1,2}/\\d{1,2}-","", rateDataCleaned$weekEnding)
rateDataCleaned$weekEnding <- sub(" \\(.*","",rateDataCleaned$weekEnding)
rateDataCleaned$weekEnding <- paste("2020",rateDataCleaned$weekEnding,sep="-")
rateDataCleaned$weekEnding <- sub("/","-",rateDataCleaned$weekEnding)
rateDataCleaned$weekEnding <- as.Date(rateDataCleaned$weekEnding)

cases100kGraph <- ggplot(rateDataCleaned,aes(weekEnding,casesPer100k))+geom_point()+geom_line()
cases100kGraph <- ggplotly(cases100kGraph,dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)%>%
  layout(title = paste('Weekly Cases per 100,000 Residents - Updated', as.Date.POSIXct(as.numeric(dateUpdated))))
htmlwidgets::saveWidget(cases100kGraph, file="../graphs/WEEKLY_100k.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title="weekly100k")
