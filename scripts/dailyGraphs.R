library(tidyverse)
library(plotly)
library(zoo)
library(htmlwidgets)
library(googlesheets4)

source("FUNCTIONS.R")

# Read data

gs4_deauth()
data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4/edit#gid=1592746937", sheet = "Trends")
data_previous <- readRDS("data/data_raw.rds") 

# Process and create graphs

if(identical(data_previous,data_raw)){
  stop("Graphs already up to date")
}else {
data_cleaned <- data_raw%>%
  select(date=1,posTest=2, negTest=5,tests=7,cases=9,admissions=15,discharges=17,currentHosp=21,ICU=23,vent=24,dailyDeaths=25)%>%
  filter(row_number() >= 11)%>%
  mutate(percentPos = round((cases/tests * 100),1),
         Avg7Day_Cases = movingAvg(cases),
         Last7Days_100k = round((rollsumr(cases, 7, fill=NA,align='right')) * (100000/1059361),0),
         Avg7Day_Tests = movingAvg(tests),
         Avg7Day_Pos = movingAvg(percentPos, 1),
         Avg7Day_Adm = movingAvg(admissions),
         Avg7Day_Hosp = movingAvg(currentHosp),
         Avg7Day_ICU = movingAvg(ICU),
         Avg7Day_Vent = movingAvg(vent),
         Avg7Day_Deaths = movingAvg(dailyDeaths))

data_tests <- data_cleaned%>%
  select(date,posTest,negTest,Avg7Day_Tests,total=tests)%>%
  pivot_longer(c(posTest,negTest), names_to="result",values_to="numTests")

data_ICU <- data_cleaned%>%
  select(date, ICU, vent, Avg7Day_ICU, Avg7Day_Vent)%>%
  pivot_longer(c("ICU", "vent"), names_to="type", values_to="patients")
         
updated <- format(tail(data_cleaned$date, 1), "%B %d, %Y")
hospUpdated <- format(data_cleaned$date[nrow(data_cleaned) - 1], "%B %d, %Y")

cases <- ggplot(data_cleaned, aes(x=date, group=1, text=paste("Date: ", date,
                                                                  "<br>Cases: ", cases,
                                                                  "<br>7-Day Average: ", Avg7Day_Cases)))+
  geom_col(aes(y=cases))+
  geom_line(aes(y=Avg7Day_Cases), color="blue")+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>New Positive Cases:", numFormat(tail(data_cleaned$cases, 1))),
       x="Date", y = "New Positive Cases")
cases <- ggArgs(cases)

cases100k <- ggplot(data_cleaned, aes(x=date, y=Last7Days_100k, group=1,
                                          text=paste("Date:", date,
                                                     "<br>Cases per 100k (Last 7 Days):", Last7Days_100k)))+
  geom_line(color="blue")+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>Cases per 100,000 (Last 7 Days):", numFormat(tail(data_cleaned$Last7Days_100k, 1))),
       x="Date", y="Cases per 100,000 (Last 7 Days)")
cases100k <- thresholdText(cases100k, "100 Cases per 100k", 100, 115)%>%
  ggArgs()

tests <- ggplot(data_tests, aes(date, numTests, fill=result, group=1))+
  geom_col(aes(text=paste0("Date: ", date,
                           "<br>", c("Positive", "Negative"), ": ", numFormat(numTests),
                           "<br>Total: ", numFormat(total))))+
  geom_line(aes(y=Avg7Day_Tests, text=paste0("Date: ", date,
                                             "<br>7-Day Average: ", numFormat(Avg7Day_Tests))), color="blue")+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>Tests Performed:",numFormat(tail(data_tests$total, 1))),
       x="Date", y="Tests Performed")+
  scale_fill_brewer(name="Result", palette="Set1")
tests <- ggArgs(tests, "Negative", "Positive")

pos <- ggplot(data_cleaned,aes(date, group=1, text=paste("Date: ", date,
                                                                  "<br>Percent Pos.: ", percentPos,
                                                                  "<br>7-Day Average: ", Avg7Day_Pos)))+
  geom_col(aes(y=percentPos))+
  geom_line(aes(y=Avg7Day_Pos),color="blue")+
  labs(title = paste0("Latest Data: ", updated,
                     " \n<sup>Percent Positive: ", tail(data_cleaned$percentPos, 1), "%"),
       x="Date", y="Percent Positive")
pos <- thresholdText(pos, "5% Positive", 5, 5.5)%>%
  ggArgs()

admissions <- ggplot(data_cleaned,aes(x=date, group=1, text=paste("Date: ", date,
                                                                      "<br>Admissions: ", admissions,
                                                                      "<br>7-Day Average: ", Avg7Day_Adm)))+
  geom_col(aes(y=admissions))+
  geom_line(aes(y=Avg7Day_Adm),color='blue')+
  labs(title=paste("Latest Data:", hospUpdated,
                   "\n<sup>Hospital Admissions:", data_cleaned$admissions[nrow(data_cleaned) - 1]),
       x="Date", y="Hospital Admissions")
admissions <- thresholdText(admissions, "30 Admissions per Day (210 per Week)", 30, 32)%>%
  ggArgs()

hosp <- ggplot(data_cleaned,aes(date, group=1, text=paste("Date: ", date,
                                                                   "<br>Hospitalized: ", currentHosp,
                                                                   "<br>7-Day Average: ", Avg7Day_Hosp)))+
  geom_col(aes(y=currentHosp))+
  geom_line(aes(y=Avg7Day_Hosp),color='blue')+
  labs(title = paste("Latest Data:", hospUpdated,
                     "\n<sup>Hospitalized:", data_cleaned$currentHosp[nrow(data_cleaned) - 1]),
       x="Date", y="Hospitalized")
hosp <- ggArgs(hosp)

ICU <- ggplot(data_ICU,aes(date, patients, fill=type, group=1))+
  geom_col(aes(text=paste0("Date: ", date,
                           "<br>", c("ICU", "Ventilator"), ": ", patients)), position="dodge")+
  geom_line(aes(y=Avg7Day_ICU, text=paste("Date: ", date,
                                          "<br>7-Day Average (ICU): ", Avg7Day_ICU)), color="blue")+
  geom_line(aes(y=Avg7Day_Vent, text=paste("Date: ", date,
                                           "<br>7-Day Average (Ventilator): ", Avg7Day_Vent)),color='blue')+
  scale_fill_brewer(name="Legend",palette="Set1")+
  labs(title = paste("Latest Data:", hospUpdated,
                     "\n<sup>ICU:", data_cleaned$ICU[nrow(data_cleaned) - 1],
                     "  |   Ventilator:", data_cleaned$vent[nrow(data_cleaned) - 1]),
       x="Date", y="ICU/Ventilator")
ICU <- ggArgs(ICU, "ICU", "Ventilator")

deaths <- ggplot(data_cleaned,aes(date, group=1, text=paste("Date: ", date,
                                                                          "<br>Deaths Reported: ", dailyDeaths,
                                                                          "<br>7-Day Average: ", Avg7Day_Deaths)))+
  geom_col(aes(y=dailyDeaths))+
  geom_line(aes(y=Avg7Day_Deaths),color="blue")+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>Deaths Reported:", tail(data_cleaned$dailyDeaths, 1)),
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

saveRDS(data_raw, "data/data_raw.rds")
saveRDS(data_raw, "data/data_cleaned.rds")
write.csv(data_cleaned, "../export/data_cleaned.csv")
}
