library(tidyverse)
library(plotly)

owid_NE <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv")%>%
  filter(location=="Rhode Island" | location=="Massachusetts" |
           location=="Connecticut" | location=="New Hampshire" |
           location=="Vermont" | location=="Maine")%>%
  select(date, location, dist=total_distributed, admin=total_vaccinations, pct=share_doses_used)%>%
  arrange(date)%>%
  mutate(pct = pct * 100,
         location= state.abb[match(location,state.name)],
         date = as.Date(date))

owid_prev <- readRDS("data/owid_NE.rds")

if(identical(owid_NE, owid_prev)){
  stop("Graph already up to date.")
}else{
  saveRDS(owid_NE, "data/owid_NE.rds")
  vaccine_percent_NE <- ggplot(owid_NE, aes(date, pct, color = location, group=1, text = paste0("Date: ", date,
                                                                                           "\nState: ", location,
                                                                                           "\nAdministered: ", pct, "%")))+
    geom_line()+
    geom_line(data=owid_NE[!is.na(owid_NE$pct), ], linetype="dashed")+
    labs(title=paste0("Percent of Doses Administered (New England)\n<sup>Data from Our World in Data (",
                      as.character(tail(owid_NE$date, 1), format="%b %d, %Y"), ") | Dashed lines represent days with no data reported</sup>"),
         x = "Date", y = "% Doses Administered")+
    scale_color_brewer(palette = "Set1")
  vaccine_percent_NE <- ggplotly(vaccine_percent_NE,
                                 tooltip="text", 
                                 dynamicTicks=TRUE,
                                 originalData=FALSE)%>%
    config(displayModeBar=FALSE)

  saveRDS(vaccine_percent_NE, "../graphs/vaccine_percent_NE.rds")
  rmarkdown::render("../index.Rmd")
  write.csv(owid_NE, "../export/new_england_vaccination.csv", row.names=FALSE)
}
