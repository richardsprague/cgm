# Area Under the Curve Calculations
#

library(tidyverse)
library(lubridate)


#
#' @title Calculate Area Under the Curve of glucose values for a restricted timeframe
#' @description Returns AUC for the first timelength minutes after the start of the glucose_df
#' @param glucose_df dataframe of glucose values
#' @param timelength number of minutes to look ahead for the AUC calculation
#' @export
auc_calc <- function(glucose_df, timelength = 120) {
  x <- glucose_df %>%
             select("time",value) %>%
             dplyr::filter(.[["time"]] < first(.[["time"]]) + lubridate::minutes(timelength) ) %>%
             mutate(sec = as.numeric(.[["time"]])-as.numeric(first(.[["time"]]))) %>%
             summarise(auc = DescTools::AUC(as.numeric(sec),value)/60/60)
           x$auc

}

#
#' @title Timestamps for specific foods
#' @description dataframe of timestamps and names of food
#' @param foodlist character vector of names of food
#' @param activity_df activity dataframe
#' @export
match_food <-function(foodlist, activity_df){

}
#
#' @title Calculate glucose levels for a restricted timeframe
#' @description returns a dataframe giving all glucose values within "timelength" of a specific activity
#' @param foodlist vector of food items
#' @param activity_df activity data frame (use default activity_raw)
#' @param glucose_df dataframe of glucose values (use default glucose_raw)
#' @param timelength number of minutes after the start of the food
#' @importFrom lubridate %within%
#' @export
food_effect <- function( foodlist = c("Oatmeal","Oatmeal w/cinnamon"),
                         activity_df,
                         glucose_df,
                         timelength = lubridate::hours(2)){
  food_df <- activity_df %>% dplyr::filter(Comment %in% foodlist)
  food_df$Comment <-
    paste0(food_df$Comment,"(",lubridate::date(food_df$Start) %>% strftime(format = "%m%d"),")")
  food_df_interval <- lubridate::interval(food_df$Start,food_df$Start + lubridate::hours(1))
  food_glucose <- glucose_df %>% dplyr::filter(apply(sapply(glucose_df$time,function(x) x %within% food_df_interval),2,any))
  f <- cbind(food_glucose[1,],experiment = "test")

  a = NULL

  for(i in food_df$Start){
    i_time <- as_datetime(i, tz = Sys.timezone())
    # < rbind(i,a)
    g <- glucose_df %>% dplyr::filter(time %within% interval(i_time - lubridate::minutes(10), i_time + timelength))
    #print(g)
    p = match(as_datetime(i),food_df$Start)
    f <- rbind(f,cbind(g,experiment = food_df$Comment[p]))
  }
  foods_experiment <- f[-1,]
  foods_experiment
}

#
#
#' @title Calculate area under the curve for each item in foodlist
#' @description returns a dataframe giving all glucose values within "timelength" of a specific activity
#' @param foodlist vector of food items
#' @param activity_df activity data frame (use default activity_raw)
#' @param glucose_df dataframe of glucose values (use default glucose_raw)
#' @param timelength number of minutes after the start of the food
#' @export
food_auc <- function(foodlist,
                     timelength = 120,
                     activity_df,
                     glucose_df
){
  food_effect(foodlist, activity_df = activity_df, glucose_df = glucose_df, timelength = minutes(timelength)) %>%
    select(time,value,experiment) %>%
    group_by(experiment) %>%
    mutate(sec = as.numeric(time)-as.numeric(first(time))) %>%
    summarise(auc = DescTools::AUC(as.numeric(sec),value)/60/60)

}

#food_auc("Latte",activity_df=activity_raw,glucose_df=glucose_raw)

#
#' @title today's date
#' @description returns a dataframe giving all glucose values within "timelength" of a specific activity.
today_is <- function() {
  lubridate::now()
}
