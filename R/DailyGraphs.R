library(tidyverse)
library(plotly)
library(zoo)
library(htmlwidgets)
library(googlesheets4)

gs4_deauth()

ICUcolors <- c("ICU" = "#ff8066", "Ventilator" = "#6685ff")

stateData <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4/edit#gid=1592746937", sheet = "Trends")
stateDataPrev <- readRDS("prev/prevState.rds") 

if(identical(stateDataPrev,stateData)){
  stop("Graphs already up to date")
}else {
saveRDS(stateData, "prev/prevState.rds")
stateDataCleaned <- stateData%>%
  select(date=1,tests=7,cases=9,currentHosp=21,ICU=23,vent=24,dailyDeaths=25)%>%
  filter(row_number() >= 11)%>%
  mutate(percentPos = round((cases/tests * 100),1),
         Avg7Day_Cases = round((rollmean(cases,7,na.pad=TRUE, align="right")), 0),
         Last7Days_100k = round((rollsumr(cases, 7, fill=NA,align='right')) * (100000/1059361),0),
         Avg7Day_Tests = round((rollmean(tests,7,na.pad=TRUE,align="right")),0),
         Avg7Day_Pos = round((rollmean(percentPos,7,na.pad=TRUE,align="right")),1),
         Avg7Day_Hosp = round((rollmean(currentHosp,7,na.pad=TRUE,align="right")),0),
         Avg7Day_ICU = round((rollmean(ICU,7,na.pad=TRUE,align="right")),0),
         Avg7Day_Vent = round((rollmean(vent,7,na.pad=TRUE,align="right")),0),
         Avg7Day_Deaths = round((rollmean(dailyDeaths,7,na.pad=TRUE,align="right")),0))

updated <- tail(stateDataCleaned$date, 1) # get last row of data frame for most recent data
updated <- format(updated, "%B %d, %Y")
hospUpdated <- stateDataCleaned$date[nrow(stateDataCleaned) - 1]
hospUpdated <- format(hospUpdated, "%B %d, %Y") # get second-to-last row as hospitalizations are a day behind

caseGraph <- ggplot(stateDataCleaned, aes(x=date, group=1, text=paste("Date: ", date,
                                                     "<br>Cases: ", cases,
                                                     "<br>7-Day Average: ", Avg7Day_Cases)))+
  geom_col(aes(y=cases))+
  geom_line(aes(y=Avg7Day_Cases), color="blue")+
  labs(title = paste("Latest data:", updated,
                     "\tCases Reported:", formatC((tail(stateDataCleaned$cases, 1)), format = "d", big.mark = ",")),
       x="Date", y = "Cases Reported")
caseGraph <- ggplotly(caseGraph,tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

case100kGraph <- ggplot(stateDataCleaned, aes(x=date, y=Last7Days_100k, group=1, text=paste("Date:", date,
                                                                       "<br>Cases per 100k (last 7 days):", Last7Days_100k)))+
                        geom_line(color="blue")+
  geom_segment(x=head(stateDataCleaned$date, 1), y = 100, xend = tail(stateDataCleaned$date, 1), yend=100, color="red")+
  geom_text(aes(x=date[116],y=100, label = "100 Cases per 100k"), color = 'red', size = 5,nudge_y=15)+
  labs(title = paste("Latest data:", updated,
                     "\tCases per 100,000 (last 7 days):",formatC((tail(stateDataCleaned$Last7Days_100k, 1)), format = "d", big.mark = ",")),
       x="Date", y="Cases per 100,000 (Last 7 days)")
case100kGraph <- ggplotly(case100kGraph,tooltip="text",dynamicTicks=TRUE,originalData=FALSE)%>%config(displayModeBar=FALSE)

testGraph <- ggplot(stateDataCleaned, aes(date, group=1, text=paste("Date: ", date,
                                                                  "<br>Tests: ", tests,
                                                                  "<br>7-Day Average: ", Avg7Day_Tests)))+
  geom_col(aes(y=tests))+
  geom_line(aes(y=Avg7Day_Tests),color="blue")+
labs(title = paste("Latest data:", updated,
                     "\tTests Performed:",formatC((tail(stateDataCleaned$tests, 1)), format = "d", big.mark = ",")),
       x="Date", y="Tests Performed")
testGraph <- ggplotly(testGraph,tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

posGraph <- ggplot(stateDataCleaned,aes(date, group=1, text=paste("Date: ", date,
                                                                  "<br>Percent Pos.: ", percentPos,
                                                                  "<br>7-Day Average: ", Avg7Day_Pos)))+
  geom_col(aes(y=percentPos))+
  geom_line(aes(y=Avg7Day_Pos),color="blue")+
  geom_segment(x=head(stateDataCleaned$date, 1), y = 5, xend = tail(stateDataCleaned$date, 1), yend=5, color="red")+
  geom_text(aes(x=date[116],y=5, label = "5% Positive"), color = 'red', size = 5,nudge_y=0.5)+
  labs(title = paste0("Latest data: ", updated,
                     " \tPercent Positive: ", tail(stateDataCleaned$percentPos, 1), "%"),
       x="Date", y="Percent Positive")
posGraph <- ggplotly(posGraph,tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

hospGraph <- ggplot(stateDataCleaned,aes(date, group=1, text=paste("Date: ", date,
                                                                   "<br>Hospitalized: ", currentHosp,
                                                                   "<br>7-Day Average: ", Avg7Day_Hosp)))+
  geom_col(aes(y=currentHosp))+
  geom_line(aes(y=Avg7Day_Hosp),color='blue')+
  labs(title = paste("Latest data:", hospUpdated,
                     "\tHospitalized:", stateDataCleaned$currentHosp[nrow(stateDataCleaned) - 1]),
       x="Date", y="Hospitalized")
hospGraph <- ggplotly(hospGraph,tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

ICUGraph <- ggplot(stateDataCleaned,aes(x=date,group=1))+
  geom_col(aes(y=ICU,fill="ICU", text=paste("Date: ", date,
                                            "<br>ICU: ", ICU,
                                            "<br>7-Day Average: ", Avg7Day_ICU)),color='red')+
  geom_line(aes(y=Avg7Day_ICU, text=paste("Date: ", date,
                                        "<br>ICU: ", ICU,
                                        "<br>7-Day Average: ", Avg7Day_ICU)),color='red')+
  geom_col(aes(y=vent,fill="Ventilator", text=paste("Date: ", date,
                                                    "<br>Ventilator: ", vent,
                                                    "<br>7-Day Average: ", Avg7Day_Vent)),color='blue')+
  geom_line(aes(y=Avg7Day_Vent, text=paste("Date: ", date,
                                           "<br>Ventilator: ", vent,
                                           "<br>7-Day Average: ", Avg7Day_Vent)),color='blue')+
  scale_fill_manual(name="Legend", labels = c("ICU", "Ventilator"),values = ICUcolors)+
  labs(title = paste("Latest data:", hospUpdated,
                     "\tICU:", stateDataCleaned$ICU[nrow(stateDataCleaned) - 1],
                     "\tVentilator:", stateDataCleaned$vent[nrow(stateDataCleaned) - 1]),
       x="Date", y="ICU/Ventilator")
ICUGraph <- ggplotly(ICUGraph,tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

dailyDeathGraph <- ggplot(stateDataCleaned,aes(date, group=1, text=paste("Date: ", date,
                                                                          "<br>Deaths: ", dailyDeaths,
                                                                          "<br>7-Day Average: ", Avg7Day_Deaths)))+
  geom_col(aes(y=dailyDeaths))+
  geom_line(aes(y=Avg7Day_Deaths),color="blue")+
  labs(title = paste("Latest data:", updated,
                     "\tDeaths:", tail(stateDataCleaned$dailyDeaths, 1)),
       x="Date", y="Deaths Reported")
dailyDeathGraph <- ggplotly(dailyDeathGraph,tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%config(displayModeBar=FALSE)

htmlwidgets::saveWidget(caseGraph, file="../graphs/cases.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailycases')
htmlwidgets::saveWidget(case100kGraph, file="../graphs/cases100k.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='cases100k')
htmlwidgets::saveWidget(testGraph, file="../graphs/tests.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailytests')
htmlwidgets::saveWidget(posGraph,file="../graphs/pos.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailypos')
htmlwidgets::saveWidget(hospGraph,file="../graphs/hosp.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailyhosp')
htmlwidgets::saveWidget(ICUGraph,file="../graphs/ICU.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailyicu')
htmlwidgets::saveWidget(dailyDeathGraph,file="../graphs/deaths.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='dailydeaths')
}
