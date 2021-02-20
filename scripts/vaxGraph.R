library(tidyverse)
library(plotly)
library(htmlwidgets)
library(zoo)
source("FUNCTIONS.R")

vaxData <- readRDS("data/vaxData.rds")

# Change python path depending on OS
if(Sys.info()['sysname'] == "Darwin"){ # Mac
  system('/usr/local/bin/python scrape.py')
}else if(Sys.info()['sysname'] == "Linux"){ # Raspberry Pi Server
  system("/usr/bin/python scrape.py")
}

vaxVector <- as.character(read.csv("data/tmpVax.csv", header=FALSE))

system("rm data/tmpVax.csv") # Remove tmp file

if(all(vaxVector==as.character(tail(vaxData,1)))){ # Check if data in graph
  stop("Latest data already scraped")
}else {
  vaxData <- rbind(vaxData, vaxVector)
  saveRDS(vaxData, "data/vaxData.rds") # Save new data file
  vaxDataCleaned <- vaxData # Process/clean data
  vaxDataCleaned$date <- as.Date(vaxDataCleaned$date, format="%b %d, %Y")
  vaxDataCleaned$totalDose1 <- as.numeric(gsub(",","",vaxDataCleaned$totalDose1))
  vaxDataCleaned$totalDose2 <- as.numeric(gsub(",","",vaxDataCleaned$totalDose2))
  vaxDataCleaned <- vaxDataCleaned %>%
    arrange(date)%>%
    mutate(dose1Only = totalDose1 - totalDose2,
           totalDoses = totalDose1+totalDose2,
           totalDosesPriorDay = totalDoses - lag(totalDoses),)
  vaxData_GRAPH <- vaxDataCleaned%>%
    select(1,4,3)%>%
    pivot_longer(c(2:3), names_to = "dose", values_to = "number")
  vaxData_GRAPH <- vaxData_GRAPH[-c(1:2),]
  
  vaccinations <- ggplot(vaxData_GRAPH, aes(date, number, fill=as.factor(dose),
                                            text = paste0("Date: ", date,
                                                          "\n", c("First Dose Only", "Fully Vaccinated"), ": ", numFormat(number))))+
    geom_col(position=position_stack(reverse=F))+
    labs(title=paste0("Last Updated: ", format(tail(vaxDataCleaned$date, 1), "%b %d, %Y"),
                      "<sup>\nFirst Dose Only: ", numFormat(tail(vaxDataCleaned$dose1Only, 1)),
                      "  |  Fully Vaccinated: ", numFormat(tail(vaxDataCleaned$totalDose2, 1)),
                      "  |  Doses Since Last Update: +", numFormat(tail(vaxDataCleaned$totalDosesPriorDay, 1))),
         margin = 30, x = "Date", y = "People Vaccinated")+
    scale_fill_brewer(name="Dose", palette="Set1")
  vaccinations <- ggArgs(vaccinations, "First Dose Only", "Fully Vaccinated")
  
  rmarkdown::render("../index.Rmd", output_format="html_document")
  
  saveRDS(vaccinations, "../graphs/vaccinations.rds")
}

