library(tidyverse)
library(plotly)
library(tigris)
library(ggthemes)
library(htmlwidgets)
library(leaflet)
library(googlesheets4)

gs4_deauth()

townData <- read_sheet("https://docs.google.com/spreadsheets/d/1c2QrNMz8pIbYEKzMJL7Uh2dtThOJa2j1sSMwiDo5Gz4/edit#gid=1592746937",
                       sheet = "Municipality")

if(identical(townDataCur,townData)){
  stop("Graphs already up to date")
}else {
  townDataCur <- townData
caseBreaks <- c(0,2500,5000,7500,10000)
graphs_out <- paste0(getwd(),'/Graphs')
hospBreaks <- c(0,200,400,600)
deathBreaks <- c(0,100,200,300)
colorMap <- c("#f2e56b","#fcba03","#f88700","#f83a00","#a61903")
colorMap2 <- c("#ffffff","#f2e56b","#fcba03","#f88700","#f83a00","#a61903")
riMAP <- county_subdivisions("Rhode Island")%>%
  left_join(townDataCur, by = c("NAME" = "Municipality of residence"))%>%
  filter(!row_number() == 8)
names(riMAP)[20] <- 'cases100k'
names(riMAP)[22] <- 'hosp100k'
names(riMAP)[24] <- 'deaths100k'
riMAP$cases100k <- as.numeric(as.character((riMAP$cases100k)))
riMAP$hosp100k <- as.numeric(as.character((riMAP$hosp100k)))
riMAP$hosp100k[is.na(riMAP$hosp100k)] <- 0
riMAP$deaths100k <- as.numeric(as.character((riMAP$deaths100k)))
riMAP$deaths100k[is.na(riMAP$deaths100k)] <- 0
caseHeatmap <- ggplot(riMAP)+geom_sf(aes(text=paste(riMAP$NAME),fill=cases100k))+
  scale_fill_gradientn(name="Cases per 100k", colors=colorMap, na.value = "grey100", breaks = caseBreaks, labels = caseBreaks)+
  labs(title="Total cases per 100,000 residents")+theme_map()
hospHeatmap <- ggplot(riMAP)+geom_sf(aes(text=paste(riMAP$NAME),fill=hosp100k))+
  scale_fill_gradientn(name="Hospitalizations per 100k", colors=colorMap2, na.value = "grey100", breaks = hospBreaks, labels = hospBreaks)+
  labs(title="Total hospitalizations per 100,000 residents")+theme_map()
deathHeatmap <- ggplot(riMAP)+geom_sf(aes(text=paste(riMAP$NAME),fill=deaths100k))+
  scale_fill_gradientn(name="Deaths per 100k", colors=colorMap2, na.value = "grey100", breaks = deathBreaks, labels = deathBreaks)+
  labs(title="Total deaths per 100,000 residents")+theme_map()
caseHeatmap <- plotly::ggplotly(caseHeatmap,dynamicTicks=TRUE, originalData=FALSE)%>%
  config(displayModeBar=FALSE)%>%
  style(hoveron='fill')
hospHeatmap <- plotly::ggplotly(hospHeatmap,dynamicTicks=TRUE, originalData=FALSE)%>%
  config(displayModeBar=FALSE)%>%
  style(hoveron='fill')
deathHeatmap <- plotly::ggplotly(deathHeatmap,dynamicTicks=TRUE, originalData=FALSE)%>%
  config(displayModeBar=FALSE)%>%
  style(hoveron='fill')
htmlwidgets::saveWidget(caseHeatmap, file="../graphs/MAP_cases.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title="casemap")
htmlwidgets::saveWidget(hospHeatmap,file="../graphs/MAP_hosp.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title="hospmap")
htmlwidgets::saveWidget(deathHeatmap,file="../graphs/MAP_deaths.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title="deathmap")
}
