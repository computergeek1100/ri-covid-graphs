library(tidyverse)
library(plotly)

source("FUNCTIONS.R")

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

CDC_NE <- read.csv("https://raw.githubusercontent.com/youyanggu/covid19-cdc-vaccination-data/main/aggregated_adjusted.csv")%>%
  filter(Location == "RI" | Location == "CT" | Location == "MA" | Location == "NH" | Location == "VT" | Location == "ME")%>%
  select(date = Date, State = Location, dist = Doses_Distributed, admin = Doses_Administered)%>%
  filter(date >= "2021-01-04")%>%
  arrange(date)%>%
  mutate(pct = (round((admin / dist), 4)) * 100,
         date = as.Date(date))

CDC_NE_prev <- readRDS("data/CDC_NE.rds")

if(identical(CDC_NE, CDC_NE_prev)){
  stop("Graph already up to date.")
}else{
  saveRDS(CDC_NE, "data/CDC_NE.rds")
  vaccine_percent_NE <- ggplot(CDC_NE, aes(date, pct, color = State, group=1, text = paste0("Date: ", date,
                                                                                           "\nState: ", State,
                                                                                           "\nAdministered: ", pct, "%")))+
    geom_line()+
    geom_line(data=CDC_NE[!is.na(CDC_NE$pct), ], linetype="dashed")+
    labs(title=paste0("Percent of Doses Administered (New England)\n<sup>Data from CDC.gov (", as.character(tail(CDC_NE$date, 1), format="%b %d, %Y"), ") | Dashed lines represent days with no data reported</sup>"),
         x = "Date", y = "% Doses Administered")+
    scale_color_manual(values=cbPalette)
  vaccine_percent_NE <- ggArgs(vaccine_percent_NE)

  vaccine_percent_NE

  saveRDS(vaccine_percent_NE, "../graphs/vaccine_percent_NE.rds")

  rmarkdown::render("../index.Rmd")

  write.csv(CDC_NE, "../export/CDC_NE.csv")
}
