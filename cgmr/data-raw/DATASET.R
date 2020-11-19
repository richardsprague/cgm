## code to prepare `DATASET` dataset goes here
library(readxl)
library(tidyverse)
libre_raw <- readxl::read_excel(file.path("inst","extdata","Librelink.xlsx"))

libre_raw$`Meter Timestamp` <- lubridate::force_tz(libre_raw$`Meter Timestamp`, "US/Pacific")

activity_raw_2019 <- readxl::read_excel(file.path("inst","extdata","Activity.xlsx"),na = "NA",
                                        sheet = "2019")
activity_raw_2018 <- readxl::read_excel(file.path("inst","extdata","Activity.xlsx"), na = "NA",
                                        sheet = "2018")

activity_raw <- dplyr::full_join(activity_raw_2018,activity_raw_2019)

glucose <- libre_raw %>% select(time = "Meter Timestamp",
                                scan = "Scan Glucose(mg/dL)",
                                hist = "Historic Glucose(mg/dL)",
                                strip = "Strip Glucose(mg/dL)",
                                food = "Notes")


glucose$value <- dplyr::if_else(is.na(glucose$scan),glucose$hist,glucose$scan)
glucose_raw <- glucose

usethis::use_data(activity_raw, overwrite = TRUE)
usethis::use_data(glucose_raw, overwrite = TRUE)

