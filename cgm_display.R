# cgm_display.r  functions to display results from a CGM download
# Assumes you already have these variables (see other notes for the specific formats)
# glucose_raw: a dataframe of
# activity_raw: a dataframe of activities

library(tidyverse)
library(lubridate)


# a handy ggplot object that draws a band through the "healthy" target zones across the width of any graph:
glucose_target_gg <-   geom_rect(aes(xmin=as.POSIXct(-Inf,  origin = "1970-01-01"),
                                     xmax=as.POSIXct(Inf,  origin= "1970-01-01"),
                                     ymin=100,ymax=140),
                                 alpha = 0.01, fill = "#CCCCCC",
                                 inherit.aes = FALSE)


# show glucose levels between start and end times
cgm_display <- function(start=lubridate::now()-lubridate::hours(18),
                        end=now(),
                        activity_df=activity_raw,
                        glucose_df=glucose_raw,
                        title = "Glucose",
                        show.label = TRUE) {
  ggplot(glucose_df ,aes(x=time,y=value)) + geom_line(size=2, color = "red")+
    geom_point(stat = "identity", aes(x=time,y=strip), color = "blue")+
    glucose_target_gg +
    geom_rect(data=activity_df %>% dplyr::filter(Activity == "Sleep") %>%
                select(xmin = Start,xmax = End) %>% cbind(ymin = -Inf, ymax = Inf),
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
              fill="red",
              alpha=0.2,
              inherit.aes = FALSE) +
    geom_rect(data=activity_df %>% dplyr::filter(Activity == "Exercise") %>%
                select(xmin = Start,xmax = End),
              aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
              fill="blue",
              alpha=0.2,
              inherit.aes = FALSE) +
    geom_vline(xintercept = activity_df %>%
                 dplyr::filter(Activity == "Event" & Comment == "awake") %>% select("Start") %>% unlist(),
               color = "green") +
    geom_vline(xintercept = activity_df %>%
                 dplyr::filter(Activity == "Food") %>% select("Start") %>% unlist(),
               color = "yellow")+
    geom_text(data = activity_df %>%
                dplyr::filter(Activity == "Food") %>% select("Start","Comment") ,
              aes(x=Start,y=50, angle=90, hjust = FALSE,  label =  Comment),
              size = 6) +
    labs(title = title, subtitle = start,
         y = "Glucose (mg/dL)",
         x = "") +  theme(plot.title = element_text(size=22))+
    scale_x_datetime(limits = c(start,end),
                     date_labels = "%m/%d %H:%M",
                     timezone = "US/Pacific")

}


# returns a dataframe giving all glucose values within "timelength" of a specific activity
food_effect <- function( foodlist = c("Oatmeal","Oatmeal w cinnamon"), activity_df = activity_raw, glucose_df = glucose_raw, timelength = lubridate::hours(2)){
  #food_df <- activity_df %>% dplyr::filter(str_detect(str_to_lower(activity_df$Comment),pattern = foodname))
  food_df <- activity_df %>% dplyr::filter(Comment %in% foodlist)
  #food_df$Comment <- paste0(food_df$Comment,"(",rownames(food_df),")")
  food_df$Comment <-
    paste0(food_df$Comment,"(",date(food_df$Start) %>% strftime(format = "%m%d"),")")
  food_df_interval <- interval(food_df$Start,food_df$Start + hours(1))
  food_glucose <- glucose_df %>% dplyr::filter(apply(sapply(glucose_df$time,function(x) x %within% food_df_interval),2,any))
  # food_glucose <- glucose_df %>% dplyr::filter(sapply(glucose_df$time,function(x) x %within% food_df_interval))
  f <- cbind(food_glucose[1,],experiment = "test")

  a = NULL

  for(i in food_df$Start){
    i_time <- as_datetime(i, tz = Sys.timezone())
    # < rbind(i,a)
    g <- glucose_df %>% dplyr::filter(time %within% interval(i_time - minutes(10), i_time + timelength))
    #print(g)
    p = match(as_datetime(i),food_df$Start)
    f <- rbind(f,cbind(g,experiment = food_df$Comment[p]))
  }
  foods_experiment <- f[-1,]
  foods_experiment
}

# calculate area under the curve for each item in foodlist
food_auc <- function(foodlist = foodlist,
                     timelength = 120,
                     activity_df = activity_raw,
                     glucose_df = glucose_raw
){
  food_effect(foodlist, activity_df = activity_df, glucose_df = glucose_df, timelength = minutes(timelength)) %>%
    select(time,value,experiment) %>%
    group_by(experiment) %>%
    mutate(sec = as.numeric(time)-as.numeric(first(time))) %>%
    summarise(auc = DescTools::AUC(as.numeric(sec),value)/60/60)

}


food_compare_display <-  function(foodlist = foodlist,
                                  timelength = 120,
                                  foodtype = "food") {
  food_effect(foodlist, timelength = minutes(timelength)) %>%
    group_by(experiment) %>%
    mutate(delta = (time - min(time)) /60) %>%
    ggplot(aes(x = delta, y = value, color = experiment)) +
    geom_line(size = 2) +
    scale_x_continuous() +
    labs(title = paste("Glucose after eating",foodtype), x = "Minutes", y = "mg/dL")
}


# display plot from startTime, timeLength
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
