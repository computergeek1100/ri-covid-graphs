library(tidyverse)
library(plotly)
library(zoo)
library(htmlwidgets)
library(googlesheets4)

# Default functions

ggArgs <- function(gg) {
  return(ggplotly(gg, tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%
           config(displayModeBar=FALSE)%>%
           layout(yaxis=list(rangemode="tozero")))
}

widgetArgs <- function(plt){
  saveWidget(plt, file=paste0("../graphs/",deparse(substitute(plt)),".html"),
             selfcontained=FALSE, libdir="../graphs/plotlyJS", title=deparse(substitute(plt)))
}

# Read data

gs4_deauth()
stateData <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4/edit#gid=1592746937", sheet = "Trends")
prevState <- readRDS("data/prevState.rds") 

# Process and create graphs

if(identical(prevState,stateData)){
  stop("Graphs already up to date")
}else {
stateDataCleaned <- stateData%>%
  select(date=1,posTest=2, negTest=5,tests=7,cases=9,admissions=15,discharges=17,currentHosp=21,ICU=23,vent=24,dailyDeaths=25)%>%
  filter(row_number() >= 11)%>%
  mutate(percentPos = round((cases/tests * 100),1),
         Avg7Day_Cases = round((rollmean(cases,7,na.pad=TRUE, align="right")), 0),
         Last7Days_100k = round((rollsumr(cases, 7, fill=NA,align='right')) * (100000/1059361),0),
         Avg7Day_Tests = round((rollmean(tests,7,na.pad=TRUE,align="right")),0),
         Avg7Day_Pos = round((rollmean(percentPos,7,na.pad=TRUE,align="right")),1),
         Avg7Day_Adm = round((rollmean(admissions,7,na.pad=TRUE,align='right')),0),
         Avg7Day_Hosp = round((rollmean(currentHosp,7,na.pad=TRUE,align="right")),0),
         Avg7Day_ICU = round((rollmean(ICU,7,na.pad=TRUE,align="right")),0),
         Avg7Day_Vent = round((rollmean(vent,7,na.pad=TRUE,align="right")),0),
         Avg7Day_Deaths = round((rollmean(dailyDeaths,7,na.pad=TRUE,align="right")),0))

testData <- stateDataCleaned%>%
  select(date,posTest,negTest,Avg7Day_Tests,total=tests)%>%
  pivot_longer(c(posTest,negTest), names_to="result",values_to="numTests")

ICUData <- stateDataCleaned%>%
  select(date, ICU, vent, Avg7Day_ICU, Avg7Day_Vent)%>%
  pivot_longer(c("ICU", "vent"), names_to="type", values_to="patients")
         
updated <- format(tail(stateDataCleaned$date, 1), "%B %d, %Y")
hospUpdated <- format(stateDataCleaned$date[nrow(stateDataCleaned) - 1], "%B %d, %Y")

cases <- ggplot(stateDataCleaned, aes(x=date, group=1, text=paste("Date: ", date,
                                                                      "<br>Cases: ", cases,
                                                                      "<br>7-Day Average: ", Avg7Day_Cases)))+
  geom_col(aes(y=cases))+
  geom_line(aes(y=Avg7Day_Cases), color="blue")+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>New Positive Cases:", formatC((tail(stateDataCleaned$cases, 1)), format = "d", big.mark = ",")),
       x="Date", y = "New Positive Cases")
cases <- ggArgs(cases)

cases100k <- ggplot(stateDataCleaned, aes(x=date, y=Last7Days_100k, group=1, text=paste("Date:", date,
                                                                       "<br>Cases per 100k (Last 7 Days):", Last7Days_100k)))+
  geom_line(color="blue")+
  geom_segment(x=head(stateDataCleaned$date, 1), y = 100, xend = tail(stateDataCleaned$date, 1), yend=100, color="red")+
  annotate("text", x=stateDataCleaned$date[146],y=115, label = "100 Cases per 100k", color = 'red', size = 5)+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>Cases per 100,000 (Last 7 Days):",formatC((tail(stateDataCleaned$Last7Days_100k, 1)), format = "d", big.mark = ",")),
       x="Date", y="Cases per 100,000 (Last 7 Days)")
cases100k <- ggArgs(cases100k)

tests <- ggplot(testData, aes(date, numTests, fill=result, group=1))+
  geom_col(aes(text=paste0("Date: ", date,
                           "<br>", c("Positive", "Negative"), ": ", formatC(numTests, format = "d", big.mark = ","),
                           "<br>Total Tests: ", formatC(total, format = "d", big.mark = ","))))+
  geom_line(aes(y=Avg7Day_Tests, text=paste0("Date: ", date,
                                             "<br>7-Day Average: ", formatC(Avg7Day_Tests, format = "d", big.mark = ","))), color="blue")+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>Tests Performed:",formatC((tail(testData$total, 1)), format = "d", big.mark = ",")),
       x="Date", y="Tests Performed")+
  scale_fill_brewer(name="Result", palette="Set1")
tests <- ggArgs(tests)
tests$x$data[[1]]$name <- "Negative"
tests$x$data[[2]]$name <- "Positive"

pos <- ggplot(stateDataCleaned,aes(date, group=1, text=paste("Date: ", date,
                                                                  "<br>Percent Pos.: ", percentPos,
                                                                  "<br>7-Day Average: ", Avg7Day_Pos)))+
  geom_col(aes(y=percentPos))+
  geom_line(aes(y=Avg7Day_Pos),color="blue")+
  geom_segment(x=head(stateDataCleaned$date, 1), y = 5, xend = tail(stateDataCleaned$date, 1), yend=5, color="red")+
  annotate("text", x=stateDataCleaned$date[146],y=5.5, label = "5% Positive", color = 'red', size = 5)+
  labs(title = paste0("Latest Data: ", updated,
                     " \n<sup>Percent Positive: ", tail(stateDataCleaned$percentPos, 1), "%"),
       x="Date", y="Percent Positive")
pos <- ggArgs(pos)

admissions <- ggplot(stateDataCleaned,aes(x=date, group=1, text=paste("Date: ", date,
                                                                                   "<br>Admissions: ", admissions,
                                                                                   "<br>7-Day Average: ", Avg7Day_Adm)))+
  geom_col(aes(y=admissions))+
  geom_line(aes(y=Avg7Day_Adm),color='blue')+
  geom_segment(x=head(stateDataCleaned$date, 1), y = 30, xend = (tail(stateDataCleaned$date, 1) - 1), yend=30, color="red")+
  annotate("text", x=stateDataCleaned$date[146],y=32, label = "30 Admissions per Day (210 per Week)", color = 'red', size = 5)+
  labs(title=paste("Latest Data:", hospUpdated,
                   "\n<sup>Hospital Admissions:", stateDataCleaned$admissions[nrow(stateDataCleaned) - 1]),
       x="Date", y="Hospital Admissions")
admissions <- ggArgs(admissions)

hosp <- ggplot(stateDataCleaned,aes(date, group=1, text=paste("Date: ", date,
                                                                   "<br>Hospitalized: ", currentHosp,
                                                                   "<br>7-Day Average: ", Avg7Day_Hosp)))+
  geom_col(aes(y=currentHosp))+
  geom_line(aes(y=Avg7Day_Hosp),color='blue')+
  labs(title = paste("Latest Data:", hospUpdated,
                     "\n<sup>Hospitalized:", stateDataCleaned$currentHosp[nrow(stateDataCleaned) - 1]),
       x="Date", y="Hospitalized")
hosp <- ggArgs(hosp)

ICU <- ggplot(ICUData,aes(date, patients, fill=type, group=1))+
  geom_col(aes(text=paste0("Date: ", date,
                           "<br>", c("ICU", "Ventilator"), ": ", patients)), position="dodge")+
  geom_line(aes(y=Avg7Day_ICU, text=paste("Date: ", date,
                                          "<br>7-Day Average (ICU): ", Avg7Day_ICU)), color="blue")+
  geom_line(aes(y=Avg7Day_Vent, text=paste("Date: ", date,
                                           "<br>7-Day Average (Ventilator): ", Avg7Day_Vent)),color='blue')+
  scale_fill_brewer(name="Legend",palette="Set1")+
  labs(title = paste("Latest Data:", hospUpdated,
                     "\n<sup>ICU:", stateDataCleaned$ICU[nrow(stateDataCleaned) - 1],
                     "  |   Ventilator:", stateDataCleaned$vent[nrow(stateDataCleaned) - 1]),
       x="Date", y="ICU/Ventilator")
ICU <- ggArgs(ICU)
ICU$x$data[[1]]$name <- "ICU"
ICU$x$data[[2]]$name <- "Ventilator"

deaths <- ggplot(stateDataCleaned,aes(date, group=1, text=paste("Date: ", date,
                                                                          "<br>Deaths Reported: ", dailyDeaths,
                                                                          "<br>7-Day Average: ", Avg7Day_Deaths)))+
  geom_col(aes(y=dailyDeaths))+
  geom_line(aes(y=Avg7Day_Deaths),color="blue")+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>Deaths Reported:", tail(stateDataCleaned$dailyDeaths, 1)),
       x="Date", y="Deaths Reported")
deaths <- ggArgs(deaths)

widgetArgs(cases)
widgetArgs(cases100k)
widgetArgs(tests)
widgetArgs(pos)
widgetArgs(admissions)
widgetArgs(hosp)
widgetArgs(ICU)
widgetArgs(deaths)

saveRDS(stateData, "data/prevState.rds")
}
