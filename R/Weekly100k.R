library(tidyverse)
library(plotly)
library(ggthemes)
library(lubridate)
library(htmlwidgets)
library(googlesheets4)

rateData <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4/edit#gid=1592746937", sheet = "Rate Trends by Age")

if(identical(rateDataCur,rateData)){
  stop("Graphs already up to date")
}else {
rateDataCur <- rateData
rateDataCleaned <- rateDataCur%>%
  select(weekEnding=1,casesPer100k=14)%>%
  filter(row_number() >= 8)
rateDataCleaned$casesPer100k <- as.numeric(as.character(rateDataCleaned$casesPer100k))
rateDataCleaned$weekEnding <- sub("\\d{1,2}/\\d{1,2}-","", rateDataCleaned$weekEnding)
rateDataCleaned$weekEnding <- sub(" \\(.*","",rateDataCleaned$weekEnding)
if(nrow(rateDataCleaned) < 44){
  rateDataCleaned$weekEnding <- paste("2020",rateDataCleaned$weekEnding,sep="-")
}else{
  for(row in 1:43(rateDataCleaned)){
    rateDataCleaned$weekEnding[row] <- paste("2020",rateDataCleaned$weekEnding[row],sep="-")
  }
  for(row in 44:nrow(rateDataCleaned)){
    rateDataCleaned$weekEnding[row] <- paste("2021",rateDataCleaned$weekEnding[row],sep="-")
  }
}
rateDataCleaned$weekEnding <- sub("/","-",rateDataCleaned$weekEnding)
rateDataCleaned$weekEnding <- as.Date(rateDataCleaned$weekEnding)

cases100kGraph <- ggplot(rateDataCleaned,aes(weekEnding,casesPer100k, text=paste("Date: ", weekEnding,
                                                                                 "Weekly Cases per 100k: ", casesPer100k)))+
  geom_line(color="blue")+
  labs(x="Week Ending", y = "Weekly Cases per 100,000 Residents")
cases100kGraph <- ggplotly(cases100kGraph,tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)
htmlwidgets::saveWidget(cases100kGraph, file="../graphs/WEEKLY_100k.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title="weekly100k")
}
