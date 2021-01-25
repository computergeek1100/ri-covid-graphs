library(tidyverse)
library(plotly)
library(htmlwidgets)

vaxData <- readRDS("data/vaxData.rds")

# change python path for macOS (Darwin) or Raspbian (Linux)
if(Sys.info()['sysname'] == "Darwin"){
  system('/usr/local/bin/python scrape.py')
}else if(Sys.info()['sysname'] == "Linux"){
  system("/usr/bin/python scrape.py")
}

vaxVector <- as.character(read.csv("data/tmpVax.csv", header=FALSE))

system("rm data/tmpVax.csv")

if(all(vaxVector==as.character(tail(vaxData,1)))){
  stop("Latest data already scraped")
}else {
  vaxData <- rbind(vaxData, vaxVector)
  saveRDS(vaxData, "data/vaxData.rds")
  vaxDataCleaned <- vaxData
  vaxDataCleaned$date <- as.Date(vaxDataCleaned$date, format="%b %d, %Y")
  vaxDataCleaned$totalDose1 <- as.numeric(gsub(",","",vaxDataCleaned$totalDose1))
  vaxDataCleaned$totalDose2 <- as.numeric(gsub(",","",vaxDataCleaned$totalDose2))
  vaxDataCleaned <- vaxDataCleaned %>%
    mutate(totalDoses = totalDose1+totalDose2,
           dose1PriorDay = totalDose1 - lag(totalDose1),
           dose2PriorDay = totalDose2 - lag(totalDose2),
           totalDosesPriorDay = totalDoses - lag(totalDoses))
  vaxData_GRAPH <- vaxDataCleaned%>%
    pivot_longer(c(2:7), names_to = "dose", values_to = "number")
  vaxData_GRAPH <- vaxData_GRAPH[-c(1:6),]
  vaxGraph <- ggplot(vaxData_GRAPH, aes(date, number, fill=as.factor(dose)))+
    geom_col(data=subset(vaxData_GRAPH, dose=="totalDose1" | dose=="totalDose2"), position=position_stack(reverse=T))+
    labs(title=paste0("Last Updated: ", format(tail(vaxDataCleaned$date, 1), "%b %d, %Y"),
                      "<sup>\nFirst Dose: ", "+", formatC((tail(vaxDataCleaned$dose1PriorDay, 1)), format = "d", big.mark = ","),
                      " (", formatC((tail(vaxDataCleaned$totalDose1, 1)), format = "d", big.mark = ","), " total)",
                      "\t\t\tSecond Dose: ", "+", formatC((tail(vaxDataCleaned$dose2PriorDay, 1)), format = "d", big.mark = ","),
                      " (", formatC((tail(vaxDataCleaned$totalDose2, 1)), format = "d", big.mark = ","), " total)"),
         margin = 30, x = "Date", y = "Total Doses Administered")+
    scale_fill_brewer(name="Dose", palette = "Set1")

  vaxGraph <- ggplotly(vaxGraph, dynamicTicks=TRUE, originalData=FALSE)%>%
    config(displayModeBar=FALSE)%>%
    layout(yaxis=list(rangemode="tozero"))

  htmlwidgets::saveWidget(vaxGraph, file="../graphs/vaccinations.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='vaccinations')
}
