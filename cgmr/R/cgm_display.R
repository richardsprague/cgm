# cgm_display.r  functions to display results from a CGM download
# Assumes you already have these variables (see other notes for the specific formats)
# glucose_raw: a dataframe of
# activity_raw: a dataframe of activities

library(tidyverse)
library(lubridate)
library(ggplot2)

globalVariables(c("ggplot","select","aes"))

#' @title glucose_target_gg
#' @description a handy ggplot object that draws a band through the "healthy" target zones across the width of any graph:
glucose_target_gg <-   geom_rect(aes(xmin=as.POSIXct(-Inf,  origin = "1970-01-01"),
                                     xmax=as.POSIXct(Inf,  origin= "1970-01-01"),
                                     ymin=100,ymax=140),
                                 alpha = 0.01, fill = "#CCCCCC",
                                 inherit.aes = FALSE)


#' @title CGM display
#' @description  show glucose levels between start and end times
#' @param start datetime for the start of the plot
#' @param end datetime for the end of the plot
#' @param activity_df activity dataframe
#' @param glucose_df glucose dataframe
#' @param title (optional) title for the plot
#' @param show.label (optional) show food names if TRUE
#' @export
cgm_display <- function(start=lubridate::now()-lubridate::hours(18),
                        end=lubridate::now(),
                        activity_df=activity_raw,
                        glucose_df=glucose_raw,
                        title = "Glucose",
                        show.label = TRUE) {
  ggplot(glucose_df ,aes(x=time,y=value)) + geom_line(size=2, color = "red")+
    geom_point(stat = "identity", aes(x=time,y=strip), color = "blue")+
    glucose_target_gg +
    geom_rect(data=activity_df %>% dplyr::filter(.data$Activity == "Sleep") %>%
                select(xmin = .data$Start,xmax = End) %>% cbind(ymin = -Inf, ymax = Inf),
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill="red",
              alpha=0.2,
              inherit.aes = FALSE) +
    geom_rect(data=activity_df %>% dplyr::filter(.data$Activity == "Exercise") %>%
                select(xmin = .data$Start,xmax = .data$End),
              aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
              fill="blue",
              alpha=0.2,
              inherit.aes = FALSE) +
    geom_vline(xintercept = activity_df %>%
                 dplyr::filter(.data$Activity == "Event" & .data$Comment == "awake") %>% select("Start") %>% unlist(),
               color = "green") +
    geom_vline(xintercept = activity_df %>%
                 dplyr::filter(.data$Activity == "Food") %>% select("Start") %>% unlist(),
               color = "yellow")+
    geom_text(data = activity_df %>%
                dplyr::filter(.data$Activity == "Food") %>% select("Start","Comment") ,
              aes(x=Start,y=50, angle=90, hjust = FALSE,  label =  Comment),
              size = 6) +
    labs(title = title, subtitle = start,
         y = "Glucose (mg/dL)",
         x = "") +  theme(plot.title = element_text(size=22))+
    scale_x_datetime(limits = c(start,end),
                     date_labels = "%m/%d %H:%M",
                     timezone = "US/Pacific")

}


#' @title Food compare display
#' @description  Plot foods compared against one another
#' @param foodlist character vector of the foods you want to plot
#' @param timelength time in minutes to plot
#' @param activity_df activity dataframe
#' @param glucose_df glucose dataframe
#' @param foodtype character vector of the label you want on the chart
#' @export
food_compare_display <-  function(foodlist = foodlist,
                                  timelength = 120,
                                  activity_df,
                                  glucose_df,
                                  foodtype = "food") {
  food_effect(foodlist, timelength = minutes(timelength), activity_df = activity_df, glucose_df = glucose_df) %>%
    group_by(experiment) %>%
    mutate(delta = (time - min(time)) /60) %>%
    ggplot(aes(x = delta, y = value, color = experiment)) +
    geom_line(size = 2) +
    scale_x_continuous() +
    labs(title = paste("Glucose after eating",foodtype), x = "Minutes", y = "mg/dL")
}


#' @title Start Plot
#' @description display plot from startTime, timeLength
#' @param startTime datetime for the start of the plot
#' @param timeLength number of hours to display
#' @param title title of the plot
#' @export
cgm_start_plot <- function(startTime = lubridate::now(), timeLength = 24, title = paste("Glucose",timeLength,"Hours")){
  cgm_display(startTime,
              startTime + lubridate::hours(timeLength),
              title = title,
              show.label = TRUE) + if(exists("watch_data")){
                geom_line(
                  data = watch_data %>% dplyr::filter(
                    type == "HeartRate" &
                      startDate > startTime &
                      endDate < startTime + lubridate::hours(timeLength)
                  )
                  %>% select(time = startDate, value = value),
                  inherit.aes = FALSE,
                  stat = "identity",
                  aes(x = time, y = value + 50 ),
                  color = "green"
                ) +
                  scale_y_continuous(sec.axis = sec_axis( ~ .-50,
                                                          name = "Heart Rate (bpm)"))
              }

}
