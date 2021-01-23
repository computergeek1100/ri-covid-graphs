library(tidyverse)
library(rvest)
library(plotly)
library(zoo)
library(htmlwidgets)
library(RSelenium)

colorList <- c("ICU" = "#ff8066", "Ventilator" = "#6685ff")

vaxData <- readRDS("data/vaxData.rds")
rD <- RSelenium::rsDriver(browser = "firefox",
                          extraCapabilities = list("moz:firefoxOptions" = list(
                            args = list('-p webscrape','--headless'))))
remDr <- rD$client
remDr$open()
remDr$setTimeout(type = "implicit", milliseconds = 5000)
remDr$navigate("https://datastudio.google.com/u/0/reporting/f95ea2dd-e77a-45dc-9735-f60e8581ff32/page/o9ivB")

totalDose1 <- remDr$findElement(using='xpath','/html/body/app-bootstrap/ng2-bootstrap/bootstrap/div/div/div/div/div/div[1]/div[2]/div/div[1]/div/div[1]/div/lego-report/lego-canvas-container/div/file-drop-zone/span/content-section/div[3]/canvas-component/div/div/div[1]/div/div/lego-table/div/div[3]/div[1]/div[2]')
totalDose1 <- totalDose1$getElementText()[[1]]

totalDose2 <- remDr$findElement(using='xpath','/html/body/app-bootstrap/ng2-bootstrap/bootstrap/div/div/div/div/div/div[1]/div[2]/div/div[1]/div/div[1]/div/lego-report/lego-canvas-container/div/file-drop-zone/span/content-section/div[3]/canvas-component/div/div/div[1]/div/div/lego-table/div/div[3]/div[2]/div[2]')
totalDose2 <- totalDose2$getElementText()[[1]]

dateUpdated_vax <- remDr$findElement(using="xpath",'/html/body/app-bootstrap/ng2-bootstrap/bootstrap/div/div/div/div/div/div[1]/div[2]/div/div[1]/div/div[1]/div/lego-report/lego-canvas-container/div/file-drop-zone/span/content-section/div[4]/canvas-component/div/div/div[1]/div/div/lego-table/div/div[3]/div/div[2]')
dateUpdated_vax <- dateUpdated_vax$getElementText()[[1]]

remDr$close()
rD$server$stop()

vaxVector <- c(dateUpdated_vax, totalDose1, totalDose2)

if(all(vectorTest==as.character(tail(vaxData,1)))){
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
  vaxData_GRAPH <- vaxData_GRAPH[-c(1,2),]
  vaxGraph <- ggplot(vaxData_GRAPH, aes(date, number, fill=as.factor(dose)))+
    geom_col(data=subset(vaxData_GRAPH, dose=="totalDose1" | dose=="totalDose2"), position=position_stack(reverse=T))+
    labs(title=paste0("Latest Data: ", dateUpdated_vax,
                      "<sup>\nFirst Dose: ", "+", formatC((tail(vaxDataCleaned$dose1PriorDay, 1)), format = "d", big.mark = ","), " (", totalDose1, " total)",
                      "\t\t\tSecond Dose: ", "+", formatC((tail(vaxDataCleaned$dose2PriorDay, 1)), format = "d", big.mark = ","), " (", totalDose2, " total)"),
         margin = 30, x = "Date", y = "Total Doses Administered")+
    scale_fill_brewer(name="Dose", palette = "Set1")
  vaxGraph <- ggplotly(vaxGraph,tooltip="text", dynamicTicks=TRUE, originalData=FALSE)%>%
    config(displayModeBar=FALSE)%>%
    layout(yaxis=list(rangemode="tozero"))

  htmlwidgets::saveWidget(vaxGraph, file="../graphs/vaccinations.html",selfcontained=FALSE,libdir="../graphs/plotlyJS",title='vaccinations')
}
