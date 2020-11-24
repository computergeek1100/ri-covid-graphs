library(tidyverse)
library(plotly)
library(zoo)
library(ggthemes)
library(lubridate)
library(htmlwidgets)
library(tigris)
library(leaflet)
library(googlesheets4)

ICUcolors <- c("ICU" = "red", "Ventilator" = "blue")

stateData <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4/edit#gid=1592746937", sheet = "Trends")

stateData <- stateData%>%
  select(date=1,tests=7,cases=9,currentHosp=21,ICU=23,vent=24,dailyDeaths=25)%>%
  filter(row_number() >= 11)%>%
  mutate(percentPos = round((cases/tests * 100),2))

caseGraph <- ggplot(stateData, aes(date,cases))+geom_col()+geom_line(aes(y=rollmean(cases, 7,na.pad=TRUE, align="right")),color="blue")
caseGraph <- ggplotly(caseGraph,dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

testGraph <- ggplot(stateData, aes(date,tests))+geom_col()+geom_line(aes(y=rollmean(tests, 7,na.pad=TRUE, align="right")),color="blue")
testGraph <- ggplotly(testGraph,dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

posGraph <- ggplot(stateData,aes(date,percentPos))+geom_col()+geom_line(aes(y=rollmean(percentPos,7, na.pad=TRUE, align="right")),color="blue")
posGraph <- ggplotly(posGraph,dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

hospGraph <- ggplot(stateData,aes(date,currentHosp))+geom_col()+geom_line(aes(y=rollmean(currentHosp,7,na.pad=TRUE, align="right")),color='blue')
hospGraph <- ggplotly(hospGraph,dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

ICUGraph <- ggplot(stateData,aes(date,))+geom_col(aes(,ICU,fill="ICU"))+geom_line(aes(y=rollmean(ICU,7,na.pad=TRUE,align="right")),color='red')+
  geom_col(aes(,vent,fill='Ventilator'))+geom_line(aes(y=rollmean(vent,7,na.pad=TRUE,align="right")),color='blue')+scale_color_manual(values = ICUcolors)
ICUGraph <- ggplotly(ICUGraph,dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

dailyDeathGraph <- ggplot(stateData,aes(date,dailyDeaths))+geom_col()+geom_line(aes(y=rollmean(dailyDeaths,7,na.pad=TRUE,align="right")),color='blue')
dailyDeathGraph <- ggplotly(dailyDeathGraph,dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

htmlwidgets::saveWidget(caseGraph, file="../graphs/DAILY_cases.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailycases')
htmlwidgets::saveWidget(testGraph, file="../graphs/DAILY_tests.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailytests')
htmlwidgets::saveWidget(posGraph,file="../graphs/DAILY_pos.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailypos')
htmlwidgets::saveWidget(hospGraph,file="../graphs/DAILY_hosp.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailyhosp')
htmlwidgets::saveWidget(ICUGraph,file="../graphs/DAILY_ICU.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailyicu')
htmlwidgets::saveWidget(dailyDeathGraph,file="../graphs/DAILY_deaths.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailydeaths')
