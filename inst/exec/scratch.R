#' ---
#' title: scratch.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.07.20"
#' purpose: For testing code in the library
#' ---

# TODO(David):
#     []: what if they have more than one protocol file?
#     []: the protocol file could have any extension
library(magrittr)
library(tidyverse)

# -----------------------------------------------
# Example 1 -------------------------------------
# -----------------------------------------------
project_id <- "b"
project_dir <- "~/Desktop/b_controlling_weber"
excel_file <- "~/Desktop/b/b_excel.xlsx"
protocol_file <- fs::dir_ls("~/Desktop/b/protocols/", pattern="*.gsprt")
data_dir = "~/Desktop/b/csv/"
datafile_pattern = "*.csv"
data_engine = rTSlib::habitest_read_files

# TODO(David): This doesn't work but it should...
rtslib::ts_create_project(project_id,
                          project_dir,
                          excel_file,
                          protocol_file,
                          data_dir = NULL,
                          datafile_pattern = NULL,
                          data_engine = tslib::habitest_read_file)




# -----------------------------------------------
# Example 2 -------------------------------------
# -----------------------------------------------
project_id <- "b"
project_dir <- fs::path(fs::path_expand("~/Desktop"), project_id)
excel_file <- "inst/exec/test_files/docs/ab_experiment.xlsx"
protocol_file <- "/Users/freestoned/William Paterson University/Freestone-Lab - lab/experiments/experiments/b_controlling_weber/experiment/switch_leftshort.gsprt"
data_dir <- "/Users/freestoned/William Paterson University/Freestone-Lab - lab/experiments/experiments/b_controlling_weber/data/csv"
datafile_pattern <- "*.csv"
data_engine <- rTSlib::habitest_read_file

rTSlib::ts_create_project(project_id, project_dir,
                          excel_file, protocol_file,
                          data_dir, datafile_pattern,
                          data_engine)

# What an analysis file might look like
db_path <- fs::path(project_dir, "data", "ab_database.db")
data <- db_events(db_path)

# for analysis, we should remove protocol
data %<>% select(-protocol)

pattern <- c("trial_start", "long_trial", "trial_end")
data %<>%
  group_by(subject, date) %>%
  mutate(trial = trialdef(event, pattern)) %>%
  ungroup()

data %<>% filter(trial > 0)

data %>%
  group_by(subject) %>%
  mutate(.trial = trialdef(event, c("On_WriteVariables", "Off_WriteVariables"))) %>%
  group_by(subject, .trial) %>%
  mutate()

data %<>%
  group_by(subject) %>%
  mutate(changes = any(changes_in(parameter, value, "short_cv"),
                       changes_in(parameter, value, "long_cv"),
                       changes_in(parameter, value, "short_food_probability"),
                       changes_in(parameter, value, "long_long_probability"),
                       changes_in(parameter, value, "short_food_amount"),
                       changes_in(parameter, value, "long_food_amount"))) %>%
  ungroup()


# TODO(David):
#   [x]: Need to write a trialdef function that works on the database
#   []: Need to be able to detect parameter groups and changes
#   [x]: Write code that converts codes to events

# Keep this at the end of the document.
fs::file_create(fs::path(project_dir, "src", "foo.R"))
scripts <-  fs::dir_ls(fs::path(project_dir, "src"))
db_add_script(db_path, scripts)

