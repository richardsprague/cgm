# cgm_load.R  # load data into dataframes useful for the rest of the functions

# assumes you have a config.yml file somewhere on your parent directory path.

library(tidyverse)
library(dplyr)
library(DBI)

library(lubridate)

library(conflicted)
conflict_prefer("filter", "dplyr")

USER_ID = 13

Sys.setenv(R_CONFIG_ACTIVE = "p4mi")

conn_args <- config::get("dataconnection")
conn_args

# first time only. Connect to the server, but not to a specific database
con <- DBI::dbConnect(drv = conn_args$driver,
                      user = conn_args$user,
                      host = conn_args$host,
                      port = conn_args$port,
                      dbname = conn_args$dbname,
                      password = conn_args$password)

# list the tables in this database
# should include "notes_records" and "glucose_records"
DBI::dbListTables(con)


glucose_df <- tbl(con, "glucoses_glucose") %>% select(-created,-modified) %>%
  filter(user_id == USER_ID & record_date > "2019-11-01") %>% collect()# & top_n(record_date,2))# %>%

glucose_new <- glucose_df %>% transmute(time = force_tz(as_datetime(record_date) + record_time, Sys.timezone()),
                                        scan = value, hist = value, strip = NA, value = value,
                                        food = as.character(stringr::str_match(notes,"Notes=.*")),
                                        user_id = user_id)

notes_records <- glucose_new %>%
  filter(!is.na(food)) %>%
  select(Start = time, Comment= food) %>%
  mutate(Activity=factor("Food"),
         Comment = as.character(Comment),
         End=NA, Z=NA)

rm(glucose_df)

#
# glucose_raw <- tbl(con,"glucose_records") %>% collect()
# activity_raw <- tbl(con,"notes_records") %>% collect()
# activity_raw$Activity <- factor(activity_raw$Activity)

DBI::dbDisconnect(con)

