# read_cgm_data.R reads all data into the correct data frames.

library(readxl)
libre_raw <- readxl::read_excel(file.path(Sys.getenv("ONEDRIVE"),"Sprague 2019","Health 2019","Librelink.xlsx"))
libre_raw$`Meter Timestamp` <- lubridate::force_tz(libre_raw$`Meter Timestamp`, "US/Pacific")

activity_raw_2019 <- readxl::read_excel(file.path(Sys.getenv("ONEDRIVE"),"Sprague 2019","Health 2019","Rik Activity 2019.xlsx"),na = "NA",
                                        sheet = "2019")
activity_raw_2018 <- readxl::read_excel(file.path(Sys.getenv("ONEDRIVE"),"Sprague 2019","Health 2019","Rik Activity 2019.xlsx"),
                                        sheet = "2018")

activity_raw <- dplyr::full_join(activity_raw_2018,activity_raw_2019)


