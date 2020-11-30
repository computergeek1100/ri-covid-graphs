library(tidyverse)
library(plotly)
library(zoo)
library(ggthemes)
library(lubridate)
library(htmlwidgets)
library(tigris)
library(leaflet)
library(googlesheets4)

ICUcolors <- c("ICU" = "#ff8066", "Ventilator" = "#6685ff")

stateData <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4/edit#gid=1592746937", sheet = "Trends")

if(identical(stateDataCur,stateData)){
  stop("Graphs already up to date")
}else {
stateDataCur <- stateData
stateDataCleaned <- stateDataCur%>%
  select(date=1,tests=7,cases=9,currentHosp=21,ICU=23,vent=24,dailyDeaths=25)%>%
  filter(row_number() >= 11)%>%
  mutate(percentPos = round((cases/tests * 100),1),
         Avg7Day_Cases = round((rollmean(cases,7,na.pad=TRUE, align="right")),0),
         Avg7Day_Tests = round((rollmean(tests,7,na.pad=TRUE,align="right")),0),
         Avg7Day_Pos = round((rollmean(percentPos,7,na.pad=TRUE,align="right")),1),
         Avg7Day_Hosp = round((rollmean(currentHosp,7,na.pad=TRUE,align="right")),0),
         Avg7Day_ICU = round((rollmean(ICU,7,na.pad=TRUE,align="right")),0),
         Avg7Day_Vent = round((rollmean(vent,7,na.pad=TRUE,align="right")),0),
         Avg7Day_Deaths = round((rollmean(dailyDeaths,7,na.pad=TRUE,align="right")),0))

caseGraph <- ggplot(stateDataCleaned, aes(x=date, group=1, text=paste("Date: ", date,
                                                     "<br>Cases: ", cases,
                                                     "<br>7-Day Average: ", Avg7Day_Cases)))+
  geom_col(aes(y=cases))+
  geom_line(aes(y=Avg7Day_Cases), color="blue")+
  labs(x="Date", y = "Cases Reported")
caseGraph <- ggplotly(caseGraph,tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

testGraph <- ggplot(stateDataCleaned, aes(date, group=1, text=paste("Date: ", date,
                                                                  "<br>Tests: ", tests,
                                                                  "<br>7-Day Average: ", Avg7Day_Tests)))+
  geom_col(aes(y=tests))+
  geom_line(aes(y=Avg7Day_Tests),color="blue")+
  labs(x="Date", y="Tests Performed")
testGraph <- ggplotly(testGraph,tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

posGraph <- ggplot(stateDataCleaned,aes(date, group=1, text=paste("Date: ", date,
                                                                  "<br>Percent Pos.: ", percentPos,
                                                                  "<br>7-Day Average: ", Avg7Day_Pos)))+
  geom_col(aes(y=percentPos))+
  geom_line(aes(y=Avg7Day_Pos),color="blue")+
  labs(x="Date", y="Percent Positive")
posGraph <- ggplotly(posGraph,tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

hospGraph <- ggplot(stateDataCleaned,aes(date, group=1, text=paste("Date: ", date,
                                                                   "<br>Hospitalized: ", currentHosp,
                                                                   "<br>7-Day Average: ", Avg7Day_Hosp)))+
  geom_col(aes(y=currentHosp))+
  geom_line(aes(y=Avg7Day_Hosp),color='blue')+
  labs(x="Date", y="Hospitalized")
hospGraph <- ggplotly(hospGraph,tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

ICUGraph <- ggplot(stateDataCleaned,aes(x=date))+
  geom_col(aes(y=ICU,fill="ICU"))+
  geom_line(aes(y=Avg7Day_ICU),color='red')+
  geom_col(aes(y=vent,fill="Ventilator"))+
  geom_line(aes(y=Avg7Day_Vent),color='blue')+
  scale_fill_manual(name="Legend", labels = c("ICU", "Ventilator"),values = ICUcolors)+
  labs(x="Date", y="ICU/Ventilator")
ICUGraph <- ggplotly(ICUGraph,dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

dailyDeathGraph <- ggplot(stateDataCleaned,aes(date))+
  geom_col(aes(y=dailyDeaths))+
  geom_line(aes(y=Avg7Day_Deaths),color="blue")+
  labs(x="Date", y="Deaths Reported")
dailyDeathGraph <- ggplotly(dailyDeathGraph,dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

htmlwidgets::saveWidget(caseGraph, file="../graphs/DAILY_cases.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailycases')
htmlwidgets::saveWidget(testGraph, file="../graphs/DAILY_tests.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailytests')
htmlwidgets::saveWidget(posGraph,file="../graphs/DAILY_pos.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailypos')
htmlwidgets::saveWidget(hospGraph,file="../graphs/DAILY_hosp.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailyhosp')
htmlwidgets::saveWidget(ICUGraph,file="../graphs/DAILY_ICU.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailyicu')
htmlwidgets::saveWidget(dailyDeathGraph,file="../graphs/DAILY_deaths.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailydeaths')
}
