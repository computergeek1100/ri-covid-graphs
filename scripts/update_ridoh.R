library(tidyverse)
library(plotly)
library(zoo)
library(googlesheets4)

source("FUNCTIONS.R")

# Read data

gs4_deauth()
state_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4/edit#gid=1592746937", sheet = "Trends")
state_previous <- readRDS("data/state_raw.rds") 

# Process and create graphs

if(identical(state_previous,state_raw)){
  stop("Graphs already up to date")
}else {
state_cleaned <- state_raw%>%
  select(date=1,posTest=2, negTest=5,tests=7,cases=9,admissions=15,currentHosp=21,ICU=23,vent=24,dailyDeaths=25,
         firstDoses_priorDay = 27, secondDoses_priorDay = 28, partial = 32, fully = 34)%>%
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
         Avg7Day_Deaths = movingAvg(dailyDeaths),
         partial=na_if(partial, "--")%>%as.numeric(),
         fully=na_if(fully, "--")%>%as.numeric(),
         firstPriorDay=na_if(firstDoses_priorDay, "--")%>%as.numeric(),
         cumulativeFirst=cumsum(replace_na(firstPriorDay, 0)),
         secondPriorDay=na_if(secondDoses_priorDay, "--")%>%as.numeric(),
         cumulativeSecond=cumsum(replace_na(secondPriorDay,0)),
         cumulativeJohnson = fully - cumulativeSecond,
         johnsonPriorDay = cumulativeJohnson - lag(cumulativeJohnson),
         firstOfTwoPriorDay = firstPriorDay - johnsonPriorDay,
         totalDoses_PriorDay = firstPriorDay + secondPriorDay,
         cumulativeFirstOnly = partial - fully,
         Avg7Day_totalDoses = round(rollmean(totalDoses_PriorDay, 7, na.pad = TRUE, align='right'), 0))%>%
  select(!c(partial, fully, firstDoses_priorDay, secondDoses_priorDay))

state_cleaned <- state_cleaned[, c(1, 5, 12, 13, 4, 14, 2:3, 11, 15, 6, 16, 7, 17, 8, 18, 9 ,19, 10, 20, 27, 26, 23, 28, 30, 29, 24, 25)]

data_tests <- state_cleaned%>%
  select(date,posTest,negTest,Avg7Day_Tests,total=tests)%>%
  pivot_longer(c(posTest,negTest), names_to="result",values_to="numTests")

data_ICU <- state_cleaned%>%
  select(date, ICU, vent, Avg7Day_ICU, Avg7Day_Vent)%>%
  pivot_longer(c("ICU", "vent"), names_to="type", values_to="patients")

data_vax <- state_cleaned%>%
  select(date, cumulativeFirstOnly, cumulativeSecond, cumulativeJohnson)%>%
  filter(date > "2020-12-13")%>%
  pivot_longer(c(cumulativeFirstOnly, cumulativeJohnson, cumulativeSecond), names_to = 'vaccine', values_to = 'doses')

data_vax_daily <- state_cleaned%>%
  select(date, firstOfTwoPriorDay, secondPriorDay, johnsonPriorDay, Avg7Day_totalDoses)%>%
  filter(date > "2020-12-13")%>%
  pivot_longer(c(firstOfTwoPriorDay, secondPriorDay, johnsonPriorDay), names_to = 'vaccine', values_to="doses")

updated <- format(tail(state_cleaned$date, 1), "%B %d, %Y")
hospUpdated <- format(state_cleaned$date[nrow(state_cleaned) - 1], "%B %d, %Y")

cases <- ggplot(state_cleaned, aes(x=date, group=1, text=paste("Date: ", dateFormat(date),
                                                               "<br>Cases: ", cases,
                                                               "<br>7-Day Average: ", Avg7Day_Cases)))+
  geom_col(aes(y=cases))+
  geom_line(aes(y=Avg7Day_Cases), color="blue")+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>New Positive Cases:", numFormat(tail(state_cleaned$cases, 1))),
       x="Date", y = "New Positive Cases")
cases <- ggArgs(cases)

cases100k <- ggplot(state_cleaned, aes(x=date, y=Last7Days_100k, group=1,
                                          text=paste("Date:", dateFormat(date),
                                                     "<br>Cases per 100k (Last 7 Days):", Last7Days_100k)))+
  geom_line(color="blue")+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>Cases per 100,000 (Last 7 Days):", numFormat(tail(state_cleaned$Last7Days_100k, 1))),
       x="Date", y="Cases per 100,000 (Last 7 Days)")
cases100k <- thresholdText(cases100k, "100 Cases per 100k", 100, 115)%>%
  ggArgs()

tests <- ggplot(data_tests, aes(date, numTests, fill=result, group=1))+
  geom_col(aes(text=paste0("Date: ", dateFormat(date),
                           "<br>", c("Positive", "Negative"), ": ", numFormat(numTests),
                           "<br>Total: ", numFormat(total))))+
  geom_line(aes(y=Avg7Day_Tests, text=paste0("Date: ", dateFormat(date),
                                             "<br>7-Day Average: ", numFormat(Avg7Day_Tests))), color="blue")+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>Tests Performed:",numFormat(tail(data_tests$total, 1))),
       x="Date", y="Tests Performed")+
  scale_fill_brewer(name="Result", palette="Set1")
tests <- ggArgs(tests, "Negative", "Positive")

pos <- ggplot(state_cleaned,aes(date, group=1, text=paste("Date: ", dateFormat(date),
                                                          "<br>Percent Pos.: ", percentPos,
                                                          "<br>7-Day Average: ", Avg7Day_Pos)))+
  geom_col(aes(y=percentPos))+
  geom_line(aes(y=Avg7Day_Pos),color="blue")+
  labs(title = paste0("Latest Data: ", updated,
                     " \n<sup>Percent Positive: ", tail(state_cleaned$percentPos, 1), "%"),
       x="Date", y="Percent Positive")
pos <- thresholdText(pos, "5% Positive", 5, 5.5)%>%
  ggArgs()

admissions <- ggplot(state_cleaned,aes(x=date, group=1, text=paste("Date: ", dateFormat(date),
                                                                   "<br>Admissions: ", admissions,
                                                                   "<br>7-Day Average: ", Avg7Day_Adm)))+
  geom_col(aes(y=admissions))+
  geom_line(aes(y=Avg7Day_Adm),color='blue')+
  labs(title=paste("Latest Data:", hospUpdated,
                   "\n<sup>Hospital Admissions:", state_cleaned$admissions[nrow(state_cleaned) - 1]),
       x="Date", y="Hospital Admissions")
admissions <- thresholdText(admissions, "30 Admissions per Day (210 per Week)", 30, 32)%>%
  ggArgs()

hosp <- ggplot(state_cleaned,aes(date, group=1, text=paste("Date: ", dateFormat(date),
                                                           "<br>Hospitalized: ", currentHosp,
                                                           "<br>7-Day Average: ", Avg7Day_Hosp)))+
  geom_col(aes(y=currentHosp))+
  geom_line(aes(y=Avg7Day_Hosp),color='blue')+
  labs(title = paste("Latest Data:", hospUpdated,
                     "\n<sup>Hospitalized:", state_cleaned$currentHosp[nrow(state_cleaned) - 1]),
       x="Date", y="Hospitalized")
hosp <- ggArgs(hosp)

ICU <- ggplot(data_ICU,aes(date, patients, fill=type, group=1))+
  geom_col(aes(text=paste0("Date: ", dateFormat(date),
                           "<br>", c("ICU", "Ventilator"), ": ", patients)), position="dodge")+
  geom_line(aes(y=Avg7Day_ICU, text=paste("Date: ", dateFormat(date),
                                          "<br>7-Day Average (ICU): ", Avg7Day_ICU)), color="blue")+
  geom_line(aes(y=Avg7Day_Vent, text=paste("Date: ", dateFormat(date),
                                           "<br>7-Day Average (Ventilator): ", Avg7Day_Vent)),color='blue')+
  scale_fill_brewer(name="Legend",palette="Set1")+
  labs(title = paste("Latest Data:", hospUpdated,
                     "\n<sup>ICU:", state_cleaned$ICU[nrow(state_cleaned) - 1],
                     "  |   Ventilator:", state_cleaned$vent[nrow(state_cleaned) - 1]),
       x="Date", y="ICU/Ventilator")
ICU <- ggArgs(ICU, "ICU", "Ventilator")

deaths <- ggplot(state_cleaned,aes(date, group=1, text=paste("Date: ", dateFormat(date),
                                                             "<br>Deaths Reported: ", dailyDeaths,
                                                             "<br>7-Day Average: ", Avg7Day_Deaths)))+
  geom_col(aes(y=dailyDeaths))+
  geom_line(aes(y=Avg7Day_Deaths),color="blue")+
  labs(title = paste("Latest Data:", updated,
                     "\n<sup>Deaths Reported:", tail(state_cleaned$dailyDeaths, 1)),
       x="Date", y="Deaths Reported")
deaths <- ggArgs(deaths)

vaccinations <- ggplot(
  data_vax,aes(date, doses, fill=factor(vaccine, levels = c("cumulativeFirstOnly", "cumulativeSecond", "cumulativeJohnson")),
               text = paste0("Date: ", dateFormat(date), "\n", c("First Dose Only: ", "Johnson & Johnson: ", "Two Doses: "), numFormat(doses))))+
  geom_col(position = position_stack(reverse=F))+
  labs(title=paste0("Latest Data: ", format(tail(state_cleaned$date, 1), "%b %d, %Y"),
                    "<sup>\nDoses Administered: ", numFormat(tail(state_cleaned$totalDoses_PriorDay, 1))),
       margin = 30, x = "Date", y = "People Vaccinated")+
  scale_fill_brewer(name="Dose", palette="Set1")

vaccinations <- ggArgs(vaccinations, "First Dose Only", "Two Doses", "Johnson & Johnson")

vaccinations_daily <- ggplot(
  data_vax_daily, aes(date, doses, fill=factor(vaccine, levels = c("firstOfTwoPriorDay", "secondPriorDay", "johnsonPriorDay")), group = 1))+
  geom_col(position = position_stack(reverse=F), aes(text = paste0("Date: ", date,
                                                                   "\n", c("First Doses: ", "Second Doses: ", "Johnson & Johnson: "),
                                                                   numFormat(doses))))+
  geom_line(aes(y = Avg7Day_totalDoses, text = paste0("Date: ", dateFormat(date), 
                                           "\n7-Day Average: ", numFormat(Avg7Day_totalDoses))), color = 'blue')+
  labs(title=paste0("Latest Data: ", format(tail(state_cleaned$date, 1), "%b %d, %Y"),
                    "<sup>\nDoses Administered: ", numFormat(tail(state_cleaned$totalDoses_PriorDay, 1))),
       margin = 30, x = "Date", y = "Doses Administered")+
  scale_fill_brewer(name="Dose",  palette="Set1")

vaccinations_daily <- ggArgs(vaccinations_daily, "First Doses", "Second Doses", "Johnson & Johnson")

saveRDS(cases, "../graphs/cases.rds")
saveRDS(cases100k, "../graphs/cases100k.rds")
saveRDS(tests, "../graphs/tests.rds")
saveRDS(pos, "../graphs/pos.rds")
saveRDS(admissions, "../graphs/admissions.rds")
saveRDS(hosp, "../graphs/hosp.rds")
saveRDS(ICU, "../graphs/ICU.rds")
saveRDS(deaths, "../graphs/deaths.rds")
saveRDS(vaccinations, "../graphs/vaccinations.rds")
saveRDS(vaccinations_daily, "../graphs/vaccinations_daily.rds")

rmarkdown::render("../index.Rmd")

saveRDS(state_raw, "data/state_raw.rds")
write.csv(state_cleaned, "../export/state_cleaned.csv", row.names = FALSE)
}
