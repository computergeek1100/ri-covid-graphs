library(tidyverse)
library(plotly)
library(htmlwidgets)
library(zoo)

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
    mutate(dose1Only = totalDose1 - totalDose2,
           totalDoses = totalDose1+totalDose2,
           totalDosesPriorDay = totalDoses - lag(totalDoses),)
  vaxData_GRAPH <- vaxDataCleaned%>%
    select(1,4,3)%>%
    pivot_longer(c(2:3), names_to = "dose", values_to = "number")
  vaxData_GRAPH <- vaxData_GRAPH[-c(1:2),]
  vaxGraph <- ggplot(vaxData_GRAPH, aes(date, number, fill=as.factor(dose), text = paste0("Date: ", date,
                                                                                          "\n", c("First Dose", "Fully Vaccinated"), ": ", formatC(number, format = "d", big.mark = ","))))+
    geom_col(position=position_stack(reverse=F))+
    labs(title=paste0("Last Updated: ", format(tail(vaxDataCleaned$date, 1), "%b %d, %Y"),
                      "<sup>\nFirst Dose Only: ", formatC((tail(vaxDataCleaned$dose1Only, 1)), format = "d", big.mark = ","),
                      "\t\t\tFully Vaccinated: ", formatC((tail(vaxDataCleaned$totalDose2, 1)), format = "d", big.mark = ","),
                      "\t\t\tDoses Administered Since Last Update: ", formatC(tail(vaxDataCleaned$totalDosesPriorDay, 1), format = "d", big.mark = ",")),
         margin = 30, x = "Date", y = "People Vaccinated")+
    scale_fill_brewer(name="Dose", palette="Set1")

  vaxGraph <- ggplotly(vaxGraph, tooltip = "text", dynamicTicks=TRUE, originalData=FALSE)%>%
    config(displayModeBar=FALSE)%>%
    layout(yaxis=list(rangemode="tozero"))
  
  vaxGraph$x$data[[1]]$name <- "First Dose Only"
  vaxGraph$x$data[[2]]$name <- "Fully Vaccinated"
  
  vaxGraph # preview graph - comment out if unnecessary

  htmlwidgets::saveWidget(vaxGraph, file="../graphs/vaccinations.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='vaccinations')
}

