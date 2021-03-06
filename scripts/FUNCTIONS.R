ggArgs <- function(gg, lab1 = NULL, lab2 = NULL, lab3 = NULL){
  plt <- ggplotly(gg, tooltip="text",dynamicTicks=TRUE, originalData=FALSE)%>%
    config(displayModeBar=FALSE)%>%
    layout(yaxis=list(rangemode="tozero"))
  if(!is.null(lab1)) plt$x$data[[1]]$name <- lab1
  if(!is.null(lab2)) plt$x$data[[2]]$name <- lab2
  if(!is.null(lab3)) plt$x$data[[3]]$name <- lab3
  plotly_build(plt)
  return(plt)
}

thresholdText <- function(plt, desc, linePos, textPos){
  plt+geom_segment(x=head(state_cleaned$date, 1), y = linePos, xend = tail(state_cleaned$date, 1), yend=linePos, color="red")+
    annotate("text", x=state_cleaned$date[round(nrow(state_cleaned)/2, 0)],y=textPos, label=desc, color = 'red', size = 5)
}

numFormat <- function(num){
  return(formatC((num), format = "d", big.mark = ","))
}

dateFormat <- function(date){
  return(format(date, "%b %d, %Y"))
}

movingAvg <- function(data, digits = 0) {
  tmp <- rollmean(data,7,na.pad=TRUE, align="right")
  return(round(tmp, digits))
}

pctChange <- function(data, digits = 2){
   return(round(100 * (data-lag(data, 7)) / lag(data, 7), digits))
}

vb_inc <- function(stat_, pct, df){
  if(tail(pct, 1) > 5){
    vb_color = "success"
    vb_icon = "fa-arrow-up"
  } else if(dplyr::between(tail(pct, 1), -5, 5)) {
    vb_color = "warning"
    vb_icon = ifelse(tail(pct, 1) > 0, "fa-arrow-up", "fa-arrow-down")
  } else if(tail(pct, 1) < -5){
    vb_color = "danger"
    vb_icon = "fa-arrow-down"
  }
  valueBox(value = paste0(tail(stat_, 1),
                          " (", ifelse(tail(pct,1) > 0, "+", "-"), (tail(pct, 1)), "%)"),
           color=vb_color, icon=vb_icon)
}

vb_dec <- function(stat_, pct, df){
  if(tail(pct, 1) < -5){
    vb_color = "success"
    vb_icon = "fa-arrow-down"
  } else if(dplyr::between(tail(pct, 1), -5, 5)) {
    vb_color = "warning"
    vb_icon = ifelse(tail(pct, 1) > 0, "fa-arrow-up", "fa-arrow-down")
  } else if(tail(pct, 1) > 5){
    vb_color = "danger"
    vb_icon = "fa-arrow-up"
  }
  valueBox(value = paste0(tail(stat_, 1),
                          " (", ifelse(tail(pct,1) > 0, "+", ""), (tail(pct, 1)), "%)"),
           color=vb_color, icon = vb_icon)
}

vb_hosp <- function(stat_, pct, df){ # Hospitalizations require extra day of lag
  if(pct[nrow(df) - 1] < -5){
    vb_color = "success"
    vb_icon = "fa-arrow-down"
  } else if(dplyr::between(pct[nrow(df) - 1], -5, 5)) {
    vb_color = "warning"
    vb_icon = ifelse(pct[nrow(df) - 1] > 0, "fa-arrow-up", "fa-arrow-down")
  } else if(pct[nrow(df) - 1] > 5){
    vb_color = "danger"
    vb_icon = "fa-arrow-up"
  }
  valueBox(value = paste0(stat_[nrow(df) - 1],
                          " (", ifelse(pct[nrow(df) - 1] > 0, "+", ""), pct[nrow(df) - 1], "%)"),
           color = vb_color, icon = vb_icon)
}