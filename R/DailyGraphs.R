library(tidyverse)
library(plotly)
library(zoo)
library(ggthemes)
library(lubridate)
library(htmlwidgets)
library(tigris)
library(leaflet)
library(googlesheets4)

stateData <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4/edit#gid=1592746937", sheet = "Trends")

stateData <- stateData%>%
  select(date=1,tests=7,cases=9,currentHosp=21,ICU=23,vent=24,dailyDeaths=25)%>%
  filter(row_number() >= 11)%>%
  mutate(percentPos = cases/tests * 100)

caseGraph <- ggplot(stateData, aes(date,cases))+geom_col()+geom_line(aes(y=rollmean(cases, 7,na.pad=TRUE)),color="blue")
caseGraph <- plotly::ggplotly(caseGraph)

posGraph <- ggplot(stateData,aes(date,percentPos))+geom_point()+geom_line(aes(y=rollmean(percentPos,7, na.pad=TRUE)),color="blue")
posGraph <- ggplotly(posGraph)

hospGraph <- ggplot(stateData,aes(date,currentHosp))+geom_point()+geom_line(aes(y=rollmean(currentHosp,7,na.pad=TRUE)),color='blue')
hospGraph <- ggplotly(hospGraph)

ICUGraph <- ggplot(stateData,aes(date,))+geom_point(aes(,ICU),color="red")+geom_line(aes(y=rollmean(ICU,7,na.pad=TRUE)),color='red')+
  geom_point(aes(,vent),color='blue')+geom_line(aes(y=rollmean(vent,7,na.pad=TRUE)),color='blue')
ICUGraph <- ggplotly(ICUGraph)

dailyDeathGraph <- ggplot(stateData,aes(date,dailyDeaths))+geom_point()+geom_line(aes(y=rollmean(dailyDeaths,7,na.pad=TRUE)),color='blue')
dailyDeathGraph <- ggplotly(dailyDeathGraph)

withr::with_dir('../graphs', saveWidget(caseGraph, file="DAILY_cases.html")+
                  saveWidget(posGraph,file="DAILY_pos.html")+
                  saveWidget(hospGraph,file="DAILY_hosp.html")+
                  saveWidget(ICUGraph,file="DAILY_ICU.html")+
                  saveWidget(dailyDeathGraph,file="DAILY_death.html"))