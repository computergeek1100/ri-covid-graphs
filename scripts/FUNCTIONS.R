ggArgs <- function(gg, lab1 = NULL, lab2 = NULL){
  plt <- ggplotly(gg, tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%
    config(displayModeBar=FALSE)%>%
    layout(yaxis=list(rangemode="tozero"))
  if(!is.null(lab1) & !is.null(lab2)){
    plt$x$data[[1]]$name <- lab1
    plt$x$data[[2]]$name <- lab2
  }
  return(plt)
}

widgetArgs <- function(plt){
  saveWidget(plt, file=paste0("../graphs/",deparse(substitute(plt)),".html"),
             selfcontained=FALSE, libdir="../graphs/plotlyJS", title=deparse(substitute(plt)))
}

thresholdText <- function(plt, desc, linePos, textPos){
  plt+geom_segment(x=head(state_cleaned$date, 1), y = linePos, xend = tail(state_cleaned$date, 1), yend=linePos, color="red")+
    annotate("text", x=state_cleaned$date[round(nrow(state_cleaned)/2, 0)],y=textPos, label=desc, color = 'red', size = 5)
}

numFormat <- function(num){
  return(formatC((num), format = "d", big.mark = ","))
}

movingAvg <- function(data, digits = 0) {
  tmp <- rollmean(data,7,na.pad=TRUE, align="right")
  return(round(tmp, digits))
}
